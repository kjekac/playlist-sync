{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Applicative
import Control.Arrow
import Control.Concurrent    (threadDelay)
import Control.Exception
import Control.Monad   --      (unless, when, forM_, void, join)
import Control.Monad.Trans.Maybe
import Data.Aeson --            (FromJSON(..), ToJSON(..), (.:), (.=), (.:?), withObject, object)
import Data.Aeson.Types      (Parser, defaultOptions, parseMaybe, parseEither)
import Data.Aeson.Key        qualified as Key
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Bifunctor
import Data.Function         (on)
import Data.Foldable         (for_,find)
import Data.Functor
import Data.Functor.Identity
import Data.List
import Data.String (IsString)
import Data.Char (Char, generalCategory, isAlphaNum, isSpace, GeneralCategory(..))
import Data.Maybe            
import Data.Ord              (Down(..))
import Data.Time.Clock       (UTCTime, getCurrentTime, addUTCTime)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Normalize as UN
import Prelude hiding (Char)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import qualified Data.Yaml as Yaml
import GHC.Generics          (Generic)
import Options.Generic
import Network.HTTP.Simple
import Network.HTTP.Base     (urlEncode)
import Network.URI           (parseURI, uriPath)
import System.Environment    (lookupEnv, getArgs, setEnv)
import System.FilePath       (takeFileName, takeExtension, (</>))
import qualified System.FilePath.Windows as W
import System.Directory.Tree qualified as DirTree
import System.IO             (hPutStrLn, stderr)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Process
import Sound.TagLib          qualified as TagLib

import System.IO.Unsafe

-- | Bidirectional pattern synonym for 'empty' and 'null' (both /O(1)/),
-- to be used together with '(:<)' or '(:>)'.
--
-- Exists in text 2.1.2 but we don't have access to that here.
-- TODO move to a compatibility module with #if MIN_VERSION_text(2,1,2)
pattern Empty :: Text
pattern Empty <- (T.null -> True) where
  Empty = T.empty

-- | Bidirectional pattern synonym for 'cons' (/O(n)/) and 'uncons' (/O(1)/),
-- to be used together with 'Empty'.
--
-- Exists in text 2.1.2 but we don't have access to that here.
-- TODO move to a compatibility module with #if MIN_VERSION_text(2,1,2)
pattern (:<) :: Char -> Text -> Text
pattern x :< xs <- (T.uncons -> Just (x, xs)) where
  (:<) = T.cons
infixr 5 :<
{-# COMPLETE Empty, (:<) #-}

-- | Bidirectional pattern synonym for 'snoc' (/O(n)/) and 'unsnoc' (/O(1)/)
-- to be used together with 'Empty'.
--
-- Exists in text 2.1.2 but we don't have access to that here.
-- TODO move to a compatibility module with #if MIN_VERSION_text(2,1,2)
pattern (:>) :: Text -> Char -> Text
pattern xs :> x <- (T.unsnoc -> Just (xs, x)) where
  (:>) = T.snoc
infixl 5 :>
{-# COMPLETE Empty, (:>) #-}


-- === domain ===

newtype Tag = Tag { getTag :: Text }
  deriving (IsString, ToJSON, Semigroup)
instance Show Tag where show = show . (.getTag)
instance Eq   Tag where (==) = on (==) (normalizeText . (.getTag))

normalizeText :: Text -> Text
normalizeText
    = T.toLower
    . squashSpaces
    . T.map mapChar
    . stripMarks
    . UN.normalize UN.NFKD
  where
    -- remove combining marks after NFKD (diacritics, etc.)
    stripMarks :: Text -> Text
    stripMarks = T.filter (\c -> generalCategory c /= NonSpacingMark)

    -- unify categories: dashes -> '-', other punct/symbols -> ' ', keep alnum, keep spaces
    mapChar :: Char -> Char
    mapChar c =
      case generalCategory c of
        DashPunctuation     -> ' '          -- all the weird hyphens
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

    squashSpaces :: Text -> Text
    squashSpaces = T.unwords . T.words

badWords :: [Text]
badWords =
  [ "remaster","remastered","anniversary","deluxe","expanded","bonus","reissue","edition","original"
  , "mono","stereo","instrumental","re-recorded","rerecorded","feat","featuring"
  ]

hasJunk :: Text -> Bool
hasJunk s = any (`T.isInfixOf` T.toLower s) badWords

-- drop bracketed segments that contain bad words: (...) and [...]
dropJunkBrackets :: Text -> Text
dropJunkBrackets = dropAll '[' ']' . dropAll '(' ')'
  where
    dropAll :: Char -> Char -> Text -> Text
    dropAll l r = go
      where
        go xs = case T.break (==l) xs of
          (_, Empty)     -> xs
          (pre, _:<rest) -> case T.break (==r) rest of
            (_, Empty)     -> xs
            (mid, _:<post) ->
              if hasJunk mid
                then pre <> go post
                else T.concat [pre, T.singleton l, mid, T.singleton r, go post]

-- if there’s a trailing " - blah" and blah has bad words, drop it
dropJunkSuffix :: Text -> Text
dropJunkSuffix s =
  case spanEnd (\c -> DashPunctuation /= (generalCategory c) && c /= '/') s of
    (Empty, _) -> s
    (a:>_, b)     -> if hasJunk b then dropJunkSuffix a else s

-- For some reason Data.Text has the monadic version but not the pure version??
spanEnd :: (Char -> Bool) -> Text -> (Text, Text)
spanEnd f = runIdentity . T.spanEndM (pure . f)

cleanQualifiers :: Text -> Text
cleanQualifiers = T.strip . dropJunkSuffix . dropJunkBrackets

cleanTag :: Text -> Tag
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
  show Track{..} = T.unpack $ T.concat [ artist.getTag, " - ", trackName.getTag
                                       , " (", album.getTag, ", ", T.pack (show duration), ")"]

instance FromJSON Track where
  parseJSON = withObject "spotify.track" $ \v -> do
    trackName  <- cleanTag <$> v .: "name"
    artist     <- Tag <$> do
      arr <- v .: "artists" :: Parser [Value]
      artists <- mapM (withObject "artist" (\ao -> ao .: "name")) arr
      pure $ T.intercalate ", " artists
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
  { filename :: FilePath
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

cancelDownload :: Download -> IO ()
cancelDownload dl = do
  let urlDelete = apiV0 <> "/transfers/downloads/" <> dl.id
      urlPost   = urlDelete <> "/cancel"
  eres <- try . httpNoBody <=< authed . setRequestMethod "DELETE" . parseRequest_ $ urlDelete
  case eres of
    Right _ -> putStrLn $ "Canceled (DELETE): " <> show dl
    Left (_ :: SomeException) -> do
      -- fallback: POST …/cancel
      putStrLn $ "DELETE failed; trying POST cancel for " <> show dl
      rsp <- httpNoBody <=< authed . setRequestMethod "POST" . parseRequest_ $ urlPost
      let status = getResponseStatusCode rsp
      if status >= 200 && status < 300
        then putStrLn $ "Canceled (POST): " <> show dl
        else putStrLn $ "Cancel failed, status " <> show status <> " for " <> show dl

-- enqueue body
newtype EnqueueRequest = EnqueueRequest { file :: FileRef }
instance ToJSON EnqueueRequest where
  toJSON (EnqueueRequest f) = toJSON [object ["filename" .= f.filename, "size" .= f.size]]

-- === config ===
apiRoot :: String
apiRoot = "http://localhost:5030"

apiV0 :: String
apiV0  = apiRoot <> "/api/v0"

-- directory where slskd writes; adjust to your slskd config
downloadsDir :: FilePath
downloadsDir = "/home/kjekac/soulseek"
-- === http helpers ===

authed :: Request -> IO Request
authed req = do
  Just key <- fmap BS.pack <$> lookupEnv "SLSKD_API_KEY"
  pure $ addRequestHeader "X-Api-Key" key req

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

trk a = trkWith "" a
trkId a = trk a a
trkWith l a b = unsafePerformIO (putStrLn (l <> show a) $> b)
trkWithId l a = trkWith l a a

isMatchOf :: Track -> Source -> Bool
isMatchOf t src =
       on T.isInfixOf normalizeText t.artist.getTag    (T.pack src.file.filename)
    && on T.isInfixOf normalizeText t.album.getTag     (T.pack $ W.takeDirectory src.file.filename)
    && on T.isInfixOf normalizeText t.trackName.getTag (T.pack $ W.takeFileName src.file.filename)
    && and (src.file.attributes >>= (.aDuration) <&> (== t.duration))


estimateKbps :: Integer -> Duration -> Int
estimateKbps bytes (Duration durSec) =
  if durSec <= 0 then 0
  else round ((fromIntegral bytes * 8) / (fromIntegral durSec * 1000) :: Double)

formatRank :: Track -> FileRef -> Int
formatRank t f =
  let ext = T.dropWhile (=='.') . T.toLower . T.pack $ takeExtension f.filename
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
        searchUrl  = apiV0 <> "/searches"
        searchText = t.artist.getTag <> " - " <> t.trackName.getTag
    putStrLn $ "searching for " <> show t
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
    results <- pollJSON (searchUrl <> "/" <> sidPath <> "/responses") 6 10000000
    putStrLn $ "results: " <> show results
    putStrLn $ "filtered: " <> show (filter (isMatchOf t) . join $ expand <$> results)
    pure . filter (isMatchOf t) . join $ expand <$> results
  where
    -- simple poller: try n times, sleep d µs between, return first successful parse
    pollJSON :: String -> Int -> Int -> IO [SearchResponse]
    pollJSON url tries delayUs = go tries
      where
        go 0 = [] <$ putStrLn ("search: no results for " <> show t)
        go n = threadDelay delayUs >> getJSON url >>= \case
          xs@(_:_) -> xs       <$ putStrLn ("got " <> show (length xs) <> " results for " <> show t)
          _        -> go (n-1) <* putStrLn ("no results, waiting: " <> show t)

-- download the first viable result; timeout each candidate; move on if stalled
download :: [Source] -> IO (Maybe FilePath)
download []     = Nothing <$ putStrLn "failed to download"
download (r:rs) = downloadOne 30 r >>= \case    -- seconds per candidate
  Just p  -> putStrLn ("Downloaded: " <> p)                    >> pure (Just p)
  Nothing -> putStrLn "Download timed out, trying next source" >> download rs

-- download :: [Source] -> IO (Maybe FilePath)
-- download srcs = runMaybeT (asum (MaybeT . downloadOne 30 <$> srcs)) >>= \case
--   Just file -> Just file <$ putStrLn ("downloaded: " <> file)
--   Nothing   -> Nothing   <$ putStrLn ("failed to download: " <> show srcs)


downloadOne :: Int -> Source -> IO (Maybe FilePath)
downloadOne seconds src = do
    putStrLn $ "trying to download " <> show src
    postJSON_ downloadsUrl $ EnqueueRequest src.file
    loop =<< Just . addUTCTime (fromIntegral seconds) <$> getCurrentTime
  where
    downloadsUrl = apiV0 <> "/transfers/downloads/" <> urlEncode src.username

    -- 'deadline' is active when we're NOT in progress. When we observe InProgress, we set it to Nothing.
    loop :: Maybe UTCTime -> IO (Maybe FilePath)
    loop mDeadline = do
      now <- getCurrentTime
      v   <- getJSON downloadsUrl
      let ds = either (const []) id $ downloadsFromEnv v
          timedOut = maybe False (now >) mDeadline

      case find relevant ds of
        Just d@Download{status = Just st, speed, bytesDone}
          | "Complete"   `isPrefixOf` st -> putStrLn "Complete" $> Just (inferPath d)
          | "Failed"     `isPrefixOf` st -> putStrLn "Failed"   $> Nothing
          | "InProgress" `isInfixOf`  st -> do
              putStrLn $ "Downloading: " <> show (speed, bytesDone)
              threadDelay 300000
              loop Nothing
        dl | timedOut  -> Nothing <$ (putStrLn "Timed out" >> for_ dl cancelDownload)
           | otherwise -> do
              let newDeadline = mDeadline <|> Just (addUTCTime (fromIntegral seconds) now)
              threadDelay 300000
              loop newDeadline


    downloadsFromEnv :: Value -> Either String [Download]
    downloadsFromEnv = parseEither $ withObject "env" \o -> do
      dirs <- o .:? "directories" .!= []                    -- [Value]
      concat <$> forM dirs (withObject "dir" $ \d -> d .:? "files" .!= [])

    inferPath :: Download -> FilePath
    inferPath d = trk ("infer from: " <> show d) $
                  let folder = W.takeFileName . W.takeDirectory $ d.filename
                      file   = W.takeFileName d.filename
                  in fromMaybe (downloadsDir </> folder </> file) d.destination

    relevant :: Download -> Bool
    relevant d = d.filename == src.file.filename && d.size == src.file.size

-- TODO fix incomplete patterns
getTrack :: DirTree.DirTree FilePath -> IO (Maybe Track)
getTrack DirTree.File{file} = Just <$> do
  Just tagFile <- TagLib.open file
  Just tag     <- TagLib.tag tagFile
  Just props   <- TagLib.audioProperties tagFile
  trackName    <- Tag . T.pack <$> TagLib.title tag
  artist       <- Tag . T.pack <$> TagLib.artist tag
  album        <- Tag . T.pack <$> TagLib.album tag
  duration     <- Duration <$> TagLib.duration props
  pure Track{..}
getTrack _ = pure Nothing

retag :: Track -> FilePath -> IO ()
retag track filePath = do
  putStrLn $ "retagging: " <> filePath
  mTagFile <- TagLib.open filePath
  case mTagFile of
    Nothing -> putStrLn "no tagfile"
    Just tagFile -> do
      putStrLn $ "tagFile: " <> show tagFile
      mTag <- TagLib.tag tagFile
      case mTag of
        Nothing -> putStrLn "no tag"
        Just tag -> do
          TagLib.setTitle tag . show $ track.trackName.getTag
          TagLib.setArtist tag . show $ track.artist.getTag
          TagLib.setAlbum tag . show $ track.album.getTag
          void $ TagLib.save tagFile
          putStrLn $ "Tags rewritten for: " ++ filePath


readPlaylist :: String -> String -> String -> IO [Track]
readPlaylist playlistURL clientId clientSecret = do
    let Just playlistId@(_:_) = takeFileName . uriPath <$> parseURI playlistURL

    spotifyToken <- getSpotifyBearer
    response <- httpJSON
                $ addRequestHeader "authorization" ("Bearer " <> spotifyToken)
                $ setRequestMethod "GET"
                $ parseRequest_ ("https://api.spotify.com/v1/playlists/" <> playlistId <> "/tracks")
    let status = getResponseStatusCode response
    unless (status >= 200 && status < 300) $ error ("getting playlist tracks failed: " <> show response)

    pure . extractSpotifyTracks . getResponseBody $ response
  where
    getSpotifyBearer :: IO BS.ByteString
    getSpotifyBearer = do
      let basic = B64.encode . BS.pack $ clientId <> ":" <> clientSecret
          req = setRequestMethod "POST"
              . setRequestBodyURLEncoded [("grant_type","client_credentials")]
              . addRequestHeader "Authorization" ("Basic " <> basic)
              . addRequestHeader "Accept" "application/json"
              $ parseRequest_ "https://accounts.spotify.com/api/token"
      Just tok <- parseMaybe (withObject "tok" (.: "access_token")) . getResponseBody <$> httpJSON req
      pure $ BS.pack tok

waitForHealth :: IO ()
waitForHealth = do
  let url = apiRoot <> "/health"
  let go 0 = fail "slskd: healthcheck timed out"
      go n = do
        req <- parseRequest url
        eres <- (Right <$> httpNoBody req) `catch` (\(_::SomeException) -> pure (Left ()))
        case eres of
          Right _ -> pure ()
          Left _  -> threadDelay 300000 >> go (n-1)
  go 2000   -- ~10m max

genConfig :: String -> Yaml.Value
genConfig key = object
  [ "web" .= object
      [ "authentication" .= object
          [ "disabled" .= False
          , "api_keys" .= object
              [ "cli" .= object
                  [ "key"  .= key
                  , "role" .= ("readwrite" :: String)
                  , "cidr" .= ("127.0.0.1/32,::1/128" :: String)
                  ]
              ]
          ]
      ]
  ]

withSlskd :: FilePath -> [FilePath] -> IO a -> IO a
withSlskd downloadsDir shared action = do
    secret <- UUID.toString <$> UUIDv4.nextRandom

    withSystemTempDirectory "slskd-" $ \tmp -> do
      let cfg  = tmp </> "slskd.yml"
          args = concat
            [ ["--config",cfg]
            , ["--headless","--http-port","5030","--no-https"]
            , ["--downloads", downloadsDir]
            , if null shared then [] else "--shared":shared
            , ["--slsk-username","piracyiscool","--slsk-password","kopimism"]
            ]
      Yaml.encodeFile cfg $ genConfig secret
      setFileMode cfg 0o600
      bracket
        -- Nix flake puts slskd on PATH
        (createProcess (proc "slskd" args))
        (\(_,_,_,ph) -> terminateProcess ph >> void (waitForProcess ph))
        (const $ waitForHealth >> setEnv "SLSKD_API_KEY" secret >> action)

data Cmd = Cmd { playlist :: String, clientId :: String, clientSecret :: String
               , downloads :: FilePath, shared :: [FilePath] }
  deriving (Show, Generic)
instance ParseRecord Cmd where parseRecord = parseRecordWithModifiers lispCaseModifiers

main :: IO ()
main = do
  Cmd{..} <- getRecord "playlist-pl"

  playlist <- readPlaylist playlist clientId clientSecret
  putStrLn $ "need tracks: " <> show (length playlist)

  existingTracks <- fmap catMaybes . mapM getTrack . DirTree.flattenDir . (.dirTree) =<< DirTree.build downloads
  putStrLn $ "have tracks: " <> show (length existingTracks)

  let missing = filter (`notElem` existingTracks) playlist
  putStrLn $ "miss tracks: " <> show (length missing)

  withSlskd downloads shared $ do
    forM_ missing $ \track -> search track >>= download . prioritize track >>= traverse (retag track)
