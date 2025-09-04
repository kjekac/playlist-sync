{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import Control.Applicative
import Control.Concurrent    (threadDelay)
import Control.Exception
import Control.Monad   --      (unless, when, forM_, void, join)
import Control.Monad.Trans.Maybe
import Data.Aeson --            (FromJSON(..), ToJSON(..), (.:), (.=), (.:?), withObject, object)
import Data.Aeson.Types      (Parser, defaultOptions, parseMaybe, parseEither)
import Data.Aeson.Key        qualified as Key
import Data.ByteString.Char8 qualified as BS
import Data.Bifunctor
import Data.Function         (on)
import Data.Foldable         (find)
import Data.Functor
import Data.List             
import Data.Maybe            
import Data.Ord              (Down(..))
import Data.Time.Clock       (UTCTime, getCurrentTime, addUTCTime)
import Data.String
import qualified Data.Text as T
import qualified Data.Text.Normalize as UN
import Data.Char (generalCategory, GeneralCategory(..), isAlphaNum, isSpace, toLower)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import GHC.Generics          (Generic)
import Network.HTTP.Simple
import Network.HTTP.Base     (urlEncode)
import System.Environment    (getEnv)
import System.FilePath       (takeFileName, takeExtension, (</>))
import System.Directory.Tree qualified as DirTree
import System.IO             (hPutStrLn, stderr)
import Sound.TagLib          qualified as TagLib

import System.IO.Unsafe

-- === domain ===

newtype Tag = Tag { getTag :: String }
  deriving (IsString, ToJSON, Semigroup)
instance Show Tag where show = (.getTag)
instance Eq   Tag where (==) = on (==) (normalizeStr . (.getTag))

-- normalizeStr :: (IsString a, Show a) => a -> a
normalizeStr
    = T.unpack
    . T.toLower
    . squashSpaces
    . T.map mapChar
    . stripMarks
    . UN.normalize UN.NFKD
    . T.pack
  where
    -- remove combining marks after NFKD (diacritics, etc.)
    stripMarks :: T.Text -> T.Text
    stripMarks = T.filter (\c -> generalCategory c /= NonSpacingMark)

    -- unify categories: dashes -> '-', other punct/symbols -> ' ', keep alnum, keep spaces
    mapChar :: Char -> Char
    mapChar c =
      case generalCategory c of
        DashPunctuation     -> '-'          -- all the weird hyphens
        ConnectorPunctuation-> ' '
        OtherPunctuation    -> ' '
        InitialQuote        -> ' '
        FinalQuote          -> ' '
        MathSymbol          -> ' '
        CurrencySymbol      -> ' '
        ModifierSymbol      -> ' '
        OtherSymbol         -> ' '
        _ | isAlphaNum c    -> c
          | isSpace c       -> ' '
          | otherwise       -> ' '

    squashSpaces :: T.Text -> T.Text
    squashSpaces = T.unwords . T.words

badWords :: [String]
badWords =
  [ "remaster","remastered","anniversary","deluxe","expanded","bonus","reissue","edition"
  , "mono","stereo","mix","remix","version","edit","radio edit","single edit","live"
  , "demo","instrumental","re-recorded","rerecorded","reimagined","remake","feat","featuring"
  ]

lowers :: String -> String
lowers = map toLower

hasBad :: String -> Bool
hasBad s = let ls = lowers s in any (`isInfixOf` ls) badWords

-- drop bracketed segments that contain bad words: (...) and [...]
dropBadBrackets :: String -> String
dropBadBrackets = dropAll '[' ']' . dropAll '(' ')'
  where
    dropAll :: Char -> Char -> String -> String
    dropAll l r = go
      where
        go xs =
          case break (== l) xs of
            (pre, [])   -> pre
            (pre, _ : rest) ->
              let (mid, rest') = break (== r) rest
                  tail' = case rest' of
                            []      -> []           -- no closing; keep what’s left
                            (_:rs)  -> rs
              in if hasBad mid then pre <> go tail'     -- drop the bracketed junk
                               else pre <> [l] <> mid <> [if null rest' then '\0' else r] <> go tail'

-- if there’s a trailing " - blah" and blah has bad words, drop it
dropBadDashSuffix :: String -> String
dropBadDashSuffix s =
  case breakOnLast " - " s of
    Nothing          -> s
    Just (a, b)      -> if hasBad b then a else s
  where
    breakOnLast :: String -> String -> Maybe (String, String)
    breakOnLast needle hay =
      let (pre, suf) = lastSplit needle hay
      in if null suf then Nothing else Just (pre, suf)
    lastSplit :: String -> String -> (String, String)
    lastSplit n xs =
      let go acc "" = acc
          go (p,q) ys =
            case stripSuffix n ys of
              Just pfx -> (pfx, drop (length n) ys)
              Nothing  -> go (init p, last p : q) (init ys)
      in go (xs, "") xs
    stripSuffix suf str =
      if suf `isSuffixOf` str then Just (take (length str - length suf) str) else Nothing
    isSuffixOf suf str = reverse suf `isPrefixOf` reverse str
    isPrefixOf pre str = take (length pre) str == pre

-- also kill inline " / " qualifiers like "Dark Train / Remastered 2014"
dropBadSlashParts :: String -> String
dropBadSlashParts =
  unwords . filter (not . hasBad) . splitOnSlash
  where
    splitOnSlash "" = []
    splitOnSlash xs =
      let (a,b) = breakOn " / " xs
      in a : case b of
               ""     -> []
               (_:_:_:rest) -> splitOnSlash rest
    breakOn n s =
      case findIndexSub n s of
        Nothing -> (s,"")
        Just i  -> (take i s, drop i s)
    findIndexSub sub s = findIndex (isPrefixOf sub) (tails s)
    isPrefixOf pre str = take (length pre) str == pre
    tails [] = [[]]
    tails ys@(_:zs) = ys : tails zs

cleanQualifiers :: String -> String
cleanQualifiers = dropBadDashSuffix . dropBadSlashParts . dropBadBrackets

cleanTag :: String -> Tag
cleanTag = Tag . cleanQualifiers

newtype Duration = Duration Integer
  deriving (FromJSON)
instance Show Duration where show (Duration d) = show d <> "s"
instance Eq   Duration where (Duration i1) == (Duration i2) = abs (i1 - i2) <= 5

data Track = Track
  { trackName :: Tag
  , artist    :: Tag
  , album     :: Tag
  , duration  :: Duration
  } deriving Eq

instance Show Track where
  show Track{..} = concat [ show artist, " - ", show trackName
                          , " (", show album, ", ", show duration, ")"]

instance FromJSON Track where
  parseJSON = withObject "spotify.track" $ \v -> do
    trackName  <- cleanTag <$> v .: "name"
    artist     <- Tag <$> do
      arr <- v .: "artists" :: Parser [Value]
      artists <- mapM (withObject "artist" (\ao -> ao .: "name")) arr
      pure $ intercalate ", " artists
    album     <- fmap cleanTag . withObject "album" (\ao -> ao .: "name") =<< (v .: "album")
    duration <- Duration . (`div` 1000) <$> v .: "duration_ms"
    pure Track{..}

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
  { aBitrate    :: Maybe Int      -- kbps (lossy only)
  , aVbr        :: Maybe Bool     -- True if VBR (lossy only)
  , aDuration   :: Maybe Duration -- seconds
  , aSampleRate :: Maybe Int      -- Hz (lossless only)
  , aBitDepth   :: Maybe Int      -- bits (lossless only)
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
  } deriving (Show)

instance FromJSON Download where
  parseJSON = withObject "Download" $ \o ->
    Download
    <$> o .:  "id"
    <*> o .:  "username"
    <*> o .:  "filename"
    <*> o .:  "size"
    <*> optional (o .: "bytesDone" <|> o .: "transferred" <|> o .: "bytesTransferred")
    <*> optional (o .: "status" <|> o .: "state")
    <*> optional (o .: "destination" <|> o .: "path" <|> o .: "localPath" <|> o .: "savePath")
    <*> o .:? "queuePosition"
    <*> o .:? "speed"

-- enqueue body
newtype EnqueueRequest = EnqueueRequest { file :: FileRef }
instance ToJSON EnqueueRequest where
  toJSON (EnqueueRequest f) = toJSON [object ["filename" .= f.filename, "size" .= f.size]]

-- === config ===

baseUrl :: String
baseUrl = "http://localhost:5030/api/v0"

-- directory where slskd writes; adjust to your slskd config
downloadsDir :: FilePath
downloadsDir = "/home/kjekac/soulseek"
-- === http helpers ===

authed :: Request -> IO Request
authed r = pure r
-- do
--   key <- getEnv "SLSKD_API_KEY"   -- export SLSKD_API_KEY=...
--   pure $ addRequestHeader "x-api-key" (BS.pack key) r

getJSON :: FromJSON a => String -> IO a
getJSON url = do
  req <- authed =<< parseRequest url
  getResponseBody <$> httpJSON req

postJSON_ :: ToJSON a => String -> a -> IO ()
postJSON_ url body = do
  req <- authed . setRequestMethod "POST" . setRequestBodyJSON body =<< parseRequest url
  rsp <- httpLbs req
  let status = getResponseStatusCode rsp
  unless (status >= 200 && status < 300) $
    fail ("post failed: " <> "\n" <> show rsp <> "\nbody:" <> show (getResponseBody rsp))

-- === matching + ranking ===

isMatchOf :: Track -> Source -> Bool
isMatchOf t src =
       on isInfixOf normalizeStr t.artist.getTag    path
    && on isInfixOf normalizeStr t.album.getTag     path
    && on isInfixOf normalizeStr t.trackName.getTag file
    && and (src.file.attributes >>= (.aDuration) <&> (== t.duration))
  where
    (file,path) = let isSep c = c == '\\' || c == '/'
                  in bimap reverse reverse . span (not . isSep) . reverse $ src.file.filename


estimateKbps :: Integer -> Duration -> Int
estimateKbps bytes (Duration durSec) =
  if durSec <= 0 then 0
  else round ((fromIntegral bytes * 8) / (fromIntegral durSec * 1000) :: Double)

formatRank :: Track -> FileRef -> Int
formatRank t f =
  let ext = dropWhile (=='.') . map toLower $ takeExtension f.filename
      attr = f.attributes
      is320cbr = maybe False (\a -> a.aVbr == Just False && maybe False (>=320) a.aBitrate) attr
      isV0     = maybe False (\a -> a.aVbr == Just True  && maybe False (>=230) a.aBitrate) attr
      est      = estimateKbps f.size t.duration
   in case ext of
        "mp3" | is320cbr   -> 100
              | est >= 310 -> 90
              | isV0       -> 80
              | est >= 220 -> 70
        "wav"              -> 60
        "flac"             -> 50
        "mp3"              -> 10
        _                  -> 40


prioritize :: Track -> [Source] -> [Source]
prioritize t = sortOn $ \src ->
  ( Down $ formatRank t src.file
  , Down src.freeUploadSlots
  , src.queueLength
  , Down src.uploadSpeed )


search :: Track -> IO [Source]
search t = do
    sid <- UUID.toString <$> UUIDv4.nextRandom
    let sidPath    = urlEncode sid
        searchUrl  = baseUrl <> "/searches"
        searchText = t.artist <> " - " <> t.trackName
    postJSON_ searchUrl $ object
      [ "id"                         .= sid
      , "searchText"                 .= searchText
      , "fileLimit"                  .= (10000 :: Int)
      , "filterResponses"            .= True
      , "includeFileAttributes"      .= True
      , "maximumPeerQueueLength"     .= (1000000 :: Int)
      , "minimumPeerUploadSpeed"     .= (0 :: Int)
      , "minimumResponseFileCount"   .= (1 :: Int)
      , "responseLimit"              .= (100 :: Int)
      , "searchTimeout"              .= (15000 :: Int)
      ]
    putStrLn $ "searching for " <> show t
    results <- pollJSON (searchUrl <> "/" <> sidPath <> "/responses") 6 10000000
    pure . filter (isMatchOf t) . join $ expand <$> results
  where
    -- simple poller: try n times, sleep d µs between, return first successful parse
    pollJSON :: String -> Int -> Int -> IO [SearchResponse]
    pollJSON url tries delayUs = go tries
      where
        go 0 = fail $ "search: no results for " <> show t
        go n = threadDelay delayUs >> getJSON url >>= \case
          xs@(_:_) -> xs       <$ putStrLn ("got " <> show (length xs) <> " results for " <> show t)
          _        -> go (n-1) <* putStrLn ("no results, waiting: " <> show t)

-- download the first viable result; timeout each candidate; move on if stalled
download :: [Source] -> IO (Maybe FilePath)
download []     = Nothing <$ putStrLn "failed to download"
download (r:rs) = downloadOne 30 r >>= \case    -- seconds per candidate
  Just p  -> putStrLn ("downloaded: " <> p)                    >> pure (Just p)
  Nothing -> putStrLn "download timed out, trying next source" >> download rs

-- download :: [Source] -> IO (Maybe FilePath)
-- download srcs = runMaybeT (asum (MaybeT . downloadOne 30 <$> srcs)) >>= \case
--   Just file -> Just file <$ putStrLn ("downloaded: " <> file)
--   Nothing   -> Nothing   <$ putStrLn ("failed to download: " <> show srcs)


downloadOne :: Int -> Source -> IO (Maybe FilePath)
downloadOne seconds src = do
    putStrLn $ "trying to download " <> show src
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
            v <- getJSON downloadsUrl
            let ds = either (const []) id $ downloadsFromEnv v
            case find relevant ds of
              Just d@Download{status=(Just st)}
                | "Complete" `isPrefixOf` st        -> putStrLn "Complete" >> (pure . Just $ inferPath d)
              Just Download{status=(Just "Failed")} -> putStrLn "Failed" >> pure Nothing
              _                                     -> putStrLn "Retry" >> threadDelay 300000 >> go

    downloadsFromEnv :: Value -> Either String [Download]
    downloadsFromEnv = parseEither $ withObject "env" \o -> do
      dirs <- o .:? "directories" .!= []                    -- [Value]
      concat <$> forM dirs (withObject "dir" $ \d -> d .:? "files" .!= [])

    inferPath :: Download -> FilePath
    inferPath d = fromMaybe (downloadsDir </> takeFileName d.filename) d.destination

    relevant :: Download -> Bool
    relevant d = d.filename == src.file.filename && d.size == src.file.size

getTrack :: DirTree.DirTree FilePath -> IO (Maybe Track)
getTrack DirTree.File{file} = Just <$> do
  Just tagFile <- TagLib.open file
  Just tag     <- TagLib.tag tagFile
  Just props   <- TagLib.audioProperties tagFile
  trackName    <- Tag <$> TagLib.title tag
  artist       <- Tag <$> TagLib.artist tag
  album        <- Tag <$> TagLib.album tag
  duration     <- Duration <$> TagLib.duration props
  pure Track{..}
getTrack _ = pure Nothing

retag :: Track -> FilePath -> IO ()
retag track filePath = do
  Just tagFile <- TagLib.open filePath
  Just tag <- TagLib.tag tagFile
  TagLib.setTitle tag $ track.trackName.getTag
  TagLib.setArtist tag $ track.artist.getTag
  TagLib.setAlbum tag $ track.album.getTag
  void $ TagLib.save tagFile
  putStrLn $ "Tags rewritten for: " ++ filePath

main :: IO ()
main = do
  let playlistId   = "6cEYRePGLzuxJ6WGL2TgAI"
  let spotifyToken = "BQBOiyEdcmCq12hPRpisASHwUo2HuVCJiuGZX3AQ9VFLCk5SbMFAjbMXHTrBItu3v8jbfWGK67BeOi9XU_Vto-KFkGrnwGeSf3_uTsYCh80e9ZQs40R2qqBkKTiVL8N8uLOgkvzoE3o"
  response <- httpJSON
              $ addRequestHeader "authorization" ("Bearer " <> spotifyToken)
              $ setRequestMethod "GET"
              $ parseRequest_ ("https://api.spotify.com/v1/playlists/" <> playlistId <> "/tracks")
  putStrLn . show $ response
  let status = getResponseStatusCode response
  unless (status >= 200 && status < 300) $ error "getting playlist tracks failed"

  existingTracks <- fmap catMaybes . mapM getTrack . DirTree.flattenDir . (.dirTree) =<< DirTree.build downloadsDir
  putStrLn $ "existing tracks: " <> show (length existingTracks)
  let missing = filter (`notElem` existingTracks) (extractSpotifyTracks $ getResponseBody response)
  putStrLn $ "missing tracks: " <> show (length missing)
  forM_ missing $ \track -> (prioritize track <$> search track) >>= download >>= traverse (retag track)
