{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Applicative   ((<|>), asum)
import Control.Concurrent    (threadDelay)
import Control.Monad         (unless, when, forM_, void, join)
import Data.Aeson --            (FromJSON(..), ToJSON(..), (.:), (.=), (.:?), withObject, object)
import Data.Aeson.Types      (Parser, defaultOptions, parseMaybe)
import Data.Aeson.Key        qualified as Key
import Data.ByteString.Char8 qualified as BS
import Data.Char             (toLower)
import Data.Foldable         (find)
import Data.List             (isInfixOf, isPrefixOf, sortOn)
import Data.Maybe            (fromMaybe, listToMaybe, mapMaybe)
import Data.Ord              (Down(..))
import Data.Time.Clock       (UTCTime, getCurrentTime, addUTCTime)
import GHC.Generics          (Generic)
import Network.HTTP.Simple
import Network.HTTP.Base     (urlEncode)
import System.Environment    (getEnv)
import System.FilePath       (takeFileName, takeExtension, (</>))
import System.Directory      (listDirectory)
import System.IO             (hPutStrLn, stderr)
import Sound.TagLib          qualified as TagLib

-- === domain ===

data Track = Track
  { trackName :: String
  , artist    :: String
  , album     :: String
  , duration  :: Integer
  } deriving (Show, Eq)

instance FromJSON Track where
  parseJSON = withObject "spotify.track" $ \v -> do
    trackName  <- v .: "name"
    artist     <- do
      arr <- v .: "artists" :: Parser [Value]
      artists <- mapM (withObject "artist" (\ao -> ao .: "name")) arr
      pure $ unwords $ map (<> ",") (init artists) ++ [last artists]
    album     <- withObject "album" (\ao -> ao .: "name") =<< (v .: "album")
    durationMs <- v .: "duration_ms"
    pure Track { trackName, artist, album, duration = durationMs `div` 1000 }

extractSpotifyTracks :: Value -> [Track]
extractSpotifyTracks =
  fromMaybe [] . parseMaybe (withObject "playlist" $ \o -> do
    items <- o .: "items"
    -- items :: [Value]
    mapM (withObject "item" $ \it -> do
      tval <- it .: "track"
      parseJSON tval) items)

-- search results (peer responses)

data FileRef = FileRef
  { filename :: String
  , size     :: Integer
  , attributes :: Maybe Attributes
  } deriving (Show)

instance FromJSON FileRef where
  parseJSON = withObject "FileRef" $ \o -> FileRef
    <$> o .:  "filename"
    <*> o .:  "size"
    <*> o .:? "attributes"

data Attributes = Attributes
  { aBitrate    :: Maybe Int     -- kbps (lossy only)
  , aVbr        :: Maybe Bool    -- True if VBR (lossy only)
  , aDuration   :: Maybe Integer -- seconds
  , aSampleRate :: Maybe Int     -- Hz (lossless only)
  , aBitDepth   :: Maybe Int     -- bits (lossless only)
  } deriving (Show, Eq)

instance FromJSON Attributes where
  parseJSON = withObject "Attributes" $ \o -> do
    let k :: Int -> Key.Key
        k n = Key.fromString $ show n

        orElse :: Parser (Maybe a) -> Parser (Maybe a) -> Parser (Maybe a)
        orElse p q = p >>= maybe q (pure . Just)

    br <- (o .:? "bitrate")    `orElse` (o .:? k 0)
    du <- (o .:? "duration")   `orElse` (o .:? k 1)
    vb <- (o .:? "vbr")
          `orElse` (fmap (== (1 :: Int)) <$> o .:? k 2)
    sr <- (o .:? "sampleRate" <|> o .:? "samplingRate") `orElse` (o .:? k 4)
    bd <- (o .:? "bitDepth")   `orElse` (o .:? k 5)
    pure (Attributes br vb du sr bd)

data SearchResponse = SearchResponse
  { fileCount       :: Int
  , files           :: [FileRef]
  , freeUploadSlots :: Int
  , queueLength     :: Integer
  , token           :: Int
  , uploadSpeed     :: Int
  , username        :: String
  } deriving (Show)

instance FromJSON SearchResponse where
  parseJSON = withObject "SearchResponse" $ \o -> SearchResponse
    <$> o .:? "fileCount"       .!= 0
    <*> o .:? "files"           .!= []
    <*> o .:? "freeUploadSlots" .!= 0
    <*> o .:? "queueLength"     .!= 0
    <*> o .:? "token"           .!= 0
    <*> o .:? "uploadSpeed"     .!= 0
    <*> o .:  "username"

data Source = Source
  { file            :: FileRef
  , freeUploadSlots :: Int
  , queueLength     :: Integer
  , token           :: Int
  , uploadSpeed     :: Int
  , username        :: String
  } deriving (Show)

expand :: SearchResponse -> [Source]
expand SearchResponse{..} = flip map files $ \file -> Source {file = file, ..}

-- transfers

data Download = Download
  { id            :: String
  , username      :: String
  , filename      :: String
  , size          :: Integer
  , bytesDone     :: Maybe Integer
  , status        :: Maybe String         -- "Queued" | "Transferring" | "Complete" | "Failed" | ...
  , destination   :: Maybe String         -- local path if exposed by your slskd
  , queuePosition :: Maybe Int
  , speed         :: Maybe Int
  } deriving (Show, Generic)
instance FromJSON Download where
  parseJSON = withObject "Download" $ \o -> Download
    <$> o .:  "id"
    <*> o .:  "username"
    <*> o .:  "filename"
    <*> o .:  "size"
    <*> (o .:? "bytesDone" <|> o .:? "transferred")
    <*> o .:? "status"
    <*> (o .:? "destination" <|> o .:? "path" <|> o .:? "localPath" <|> o .:? "savePath")
    <*> o .:? "queuePosition"
    <*> o .:? "speed"

-- enqueue body

newtype EnqueueRequest = EnqueueRequest { file :: FileRef }
instance ToJSON EnqueueRequest where
  toJSON (EnqueueRequest f) = object ["files" .= [object ["filename" .= f.filename, "size" .= f.size]]]

-- === config ===

baseUrl :: String
baseUrl = "http://localhost:5030/api/v0"

-- directory where slskd writes; adjust to your slskd config
downloadsDir :: FilePath
downloadsDir = "/srv/slskd/downloads"

-- === http helpers ===

authed :: Request -> IO Request
authed r = do
  key <- getEnv "SLSKD_API_KEY"   -- export SLSKD_API_KEY=...
  pure $ addRequestHeader "x-api-key" (BS.pack key) r

getJSON :: FromJSON a => String -> IO a
getJSON url = do
  req <- authed =<< parseRequest url
  getResponseBody <$> httpJSON req

postJSON_ :: ToJSON a => String -> a -> IO ()
postJSON_ url body = do
  req <- authed . setRequestMethod "POST" . setRequestBodyJSON body =<< parseRequest url
  rsp <- httpNoBody req
  unless (getResponseStatusCode rsp `elem` [200,202,204]) $
    fail ("post failed: " <> show (getResponseStatus rsp))

-- === matching + ranking ===

isMatchOf :: Track -> Source -> Bool
isMatchOf t src =
  let filename = toLower <$> src.file.filename
  in (toLower <$> t.artist) `isInfixOf` filename
  && (toLower <$> t.trackName) `isInfixOf` filename
  && maybe True (\d -> t.duration - 5 <= d && d <= t.duration + 5)
                (src.file.attributes >>= (.aDuration))

formatRank :: FileRef -> Int
formatRank f =
  let ext = dropWhile (=='.') . map toLower $ takeExtension f.filename
      attr = f.attributes
      is320cbr = maybe False (\a -> a.aVbr == Just False && maybe False (>=320) a.aBitrate) attr
      isV0     = maybe False (\a -> a.aVbr == Just True  && maybe False (>=230) a.aBitrate) attr
   in case ext of
        "mp3" | is320cbr -> 100
        "mp3" | isV0     ->  90
        "wav"            ->  80
        "flac"           ->  70
        "mp3"            ->  10
        _                ->  60


prioritize :: [Source] -> [Source]
prioritize = sortOn $ \r ->
  ( Down $ formatRank r.file
  , Down r.freeUploadSlots
  , r.queueLength
  , Down r.uploadSpeed )


search :: Track -> IO [Source]
search t = do
  let sid = urlEncode . show $ t
      searchUrl = baseUrl <> "/searches"
      searchText = t.artist <> " - " <> t.trackName
  postJSON_ searchUrl $ object
    [ "id"                         .= sid
    , "searchText"                 .= searchText
    , "fileLimit"                  .= (10000 :: Int)
    , "filterResponses"            .= True
    , "maximumPeerQueueLength"     .= (1000000 :: Int)
    , "minimumPeerUploadSpeed"     .= (0 :: Int)
    , "minimumResponseFileCount"   .= (1 :: Int)
    , "responseLimit"              .= (100 :: Int)
    , "searchTimeout"              .= (15000 :: Int)
    ]
  results <- pollJSON (searchUrl <> "/" <> sid <> "/responses") 6 10000000
  pure . filter (isMatchOf t) . join $ expand <$> results
 where
  -- simple poller: try n times, sleep d µs between, return first successful parse
  pollJSON :: String -> Int -> Int -> IO [SearchResponse]
  pollJSON url tries delayUs = go tries
    where
      go 0 = fail $ "search: no results for " <> show t
      go n = threadDelay delayUs >> getJSON url >>= \case
        xs@(_:_) -> pure xs
        _        -> go (n-1)

-- download the first viable result; timeout each candidate; move on if stalled
download :: [Source] -> IO FilePath
download []     = fail "no viable peers/files"
download (r:rs) = downloadOne 30 r >>= \case    -- seconds per candidate
  Just p  -> pure p
  Nothing -> download rs


downloadOne :: Int -> Source -> IO (Maybe FilePath)
downloadOne seconds src = do
    postJSON_ downloadsUrl $ EnqueueRequest src.file
    loop =<< addUTCTime (fromIntegral seconds) <$> getCurrentTime
  where
    downloadsUrl = baseUrl <> "/transfers/downloads/" <> urlEncode src.username

    loop :: UTCTime -> IO (Maybe FilePath)
    loop deadline = go
      where
        go = do
          now <- getCurrentTime
          if now > deadline then pure Nothing else do
            ds :: [Download] <- getJSON downloadsUrl
            case find relevant ds of
              Just d@Download{status=(Just st)}
                | "Complete" `isPrefixOf` st        -> pure . Just $ inferPath d -- case d.status of
              Just Download{status=(Just "Failed")} -> pure Nothing
              _                                     -> threadDelay 300000 >> go
--          Just "Complete" -> pure . Just $ inferPath d
--          Just "Failed"   -> pure Nothing
--          _               -> threadDelay 300000 >> loop

    inferPath :: Download -> FilePath
    inferPath d = fromMaybe (downloadsDir </> takeFileName d.filename) d.destination

    relevant :: Download -> Bool
    relevant d = d.filename == src.file.filename && d.size == src.file.size

getTrack :: FilePath -> IO Track
getTrack file = do
  Just tagFile <- TagLib.open file
  Just tag     <- TagLib.tag tagFile
  Just props   <- TagLib.audioProperties tagFile
  Track <$> TagLib.title tag <*> TagLib.artist tag <*> TagLib.album tag <*> TagLib.duration props

retag :: Track -> FilePath -> IO ()
retag track filePath = do
  Just tagFile <- TagLib.open filePath
  Just tag <- TagLib.tag tagFile
  TagLib.setTitle tag track.trackName
  TagLib.setArtist tag track.artist
  TagLib.setAlbum tag track.album
  void $ TagLib.save tagFile
  putStrLn $ "Tags rewritten for: " ++ filePath

main :: IO ()
main = do
  let playlistId   = undefined
  let spotifyToken = undefined
  let dir          = "path/to/folder"
  response <- httpJSON
              $ addRequestHeader "authorization" ("Bearer " <> spotifyToken)
              $ setRequestMethod "GET"
              $ parseRequest_ ("https://api.spotify.com/v1/playlists/" <> playlistId <> "/tracks")
  unless (getResponseStatusCode response `elem` [200,202,204]) $ error "getting playlist tracks failed"

  existingTracks <- mapM (getTrack . (dir </>)) =<< listDirectory dir
  let missing = filter (`notElem` existingTracks) (extractSpotifyTracks $ getResponseBody response)
  forM_ missing $ \track -> (prioritize <$> search track) >>= download >>= retag track
