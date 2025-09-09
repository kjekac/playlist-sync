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

import Control.Concurrent    (threadDelay)
import Control.Exception
import Control.Monad   --      (unless, when, forM_, void, join)
import Control.Monad.Trans.Maybe
import Data.Aeson --            (FromJSON(..), ToJSON(..), (.:), (.=), (.:?), withObject, object)
import Data.Aeson.Types      (Parser, parseMaybe, parseEither)
import Data.Aeson.Key        qualified as Key
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Base64 qualified as B64
import Data.Function         (on, (&))
import Data.Foldable         (for_)
import Data.Functor
import Data.Functor.Identity
import Data.List
import Data.Char (generalCategory, isAlphaNum, isSpace, GeneralCategory(..))
import Data.Either
import Data.Maybe            
import Data.Ord              (Down(..))
import Data.Time.Clock       (UTCTime, getCurrentTime, addUTCTime)
import qualified Data.Text as T
import qualified Data.Text.Normalize as UN
import Prelude hiding (log)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDv4
import qualified Data.Yaml as Yaml
import Options.Generic
import Network.HTTP.Simple
import Network.HTTP.Base     (urlEncode)
import Network.URI           (parseURI, uriPath)
import System.Environment    (lookupEnv, setEnv)
import System.FilePath       (takeFileName, takeExtension, (</>))
import qualified System.FilePath.Windows as W
import System.Directory.Tree qualified as DirTree
import System.Directory (getHomeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (setFileMode)
import System.Process
import Sound.HTagLib         qualified as HTagLib

log :: Show a => a -> IO ()
log msg = lookupEnv "DEBUG" >>= \case
  Just "True" -> putStrLn $ show msg
  _           -> pure ()

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

cleanTag :: Text -> Text
cleanTag = T.strip . dropJunkSuffix . dropJunkBrackets

instance FromJSON Track where
  parseJSON = withObject "spotify.track" $ \v -> do
    title  <- mkTitle <$> v .: "name"
    artist     <- mkArtist <$> do
      arr <- v .: "artists" :: Parser [Value]
      artists <- mapM (withObject "artist" (\ao -> ao .: "name")) arr
      pure $ T.intercalate ", " artists
    album     <- fmap mkAlbum . withObject "album" (\ao -> ao .: "name") =<< (v .: "album")
    duration <- mkDuration . (`div` 1000) <$> v .: "duration_ms"
    pure Track{..}

newtype Title = Title HTagLib.Title
instance Show Title where show = T.unpack . getTitle
instance Eq   Title where (==) = on (==) (normalizeText . getTitle)

newtype Artist = Artist HTagLib.Artist
instance Show Artist where show = T.unpack . getArtist
instance Eq   Artist where (==) = on (==) (normalizeText . getArtist)

newtype Album = Album HTagLib.Album
instance Show Album where show = T.unpack . getAlbum
instance Eq   Album where (==) = on (==) (normalizeText . getAlbum)

newtype Duration = Duration HTagLib.Duration
instance Show     Duration where show = ((<>) "s") . show . getDuration
instance Eq       Duration where d1 == d2 = abs (getDuration d1 - getDuration d2) <= 5
instance FromJSON Duration where parseJSON = fmap mkDuration . parseJSON

getTitle :: Title -> Text
getTitle (Title t) = HTagLib.unTitle t

getArtist :: Artist -> Text
getArtist (Artist t) = HTagLib.unArtist t

getAlbum :: Album -> Text
getAlbum (Album t) = HTagLib.unAlbum t

getDuration :: Duration -> Int
getDuration (Duration d) = HTagLib.unDuration d

mkTitle :: Text -> Title
mkTitle = Title . HTagLib.mkTitle . cleanTag

mkArtist :: Text -> Artist
mkArtist = Artist . HTagLib.mkArtist . cleanTag

mkAlbum :: Text -> Album
mkAlbum = Album . HTagLib.mkAlbum . cleanTag

mkDuration :: Int -> Duration
mkDuration = Duration . fromJust . HTagLib.mkDuration

data Track = Track
  { title    :: Title
  , artist   :: Artist
  , album    :: Album
  , duration :: Duration
  } deriving Eq

instance Show Track where
  show Track{..} = concat [ show artist, " - ", show title
                          , " (", show album, ", ", show duration, ")" ]

extractSpotifyTracks :: Value -> [Track]
extractSpotifyTracks =
  fromMaybe [] . parseMaybe (withObject "playlist" $ \o -> do
    items <- o .: "items"
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
    Right _ -> log ("Canceled (DELETE)",dl)
    Left (_ :: SomeException) -> do
      -- fallback: POST …/cancel
      log ("DELETE failed; trying POST cancel",dl)
      rsp <- httpNoBody <=< authed . setRequestMethod "POST" . parseRequest_ $ urlPost
      let status = getResponseStatusCode rsp
      if status >= 200 && status < 300
        then log ("Canceled (POST)",dl)
        else log ("Cancel failed",rsp,dl)

-- enqueue body
newtype EnqueueRequest = EnqueueRequest { file :: FileRef }
instance ToJSON EnqueueRequest where
  toJSON (EnqueueRequest f) = toJSON [object ["filename" .= f.filename, "size" .= f.size]]

-- === config ===
apiRoot :: String
apiRoot = "http://localhost:5030"

apiV0 :: String
apiV0  = apiRoot <> "/api/v0"

-- === http helpers ===

authed :: Request -> IO Request
authed req = do
  Just key <- fmap BS.pack <$> lookupEnv "SLSKD_API_KEY"
  pure $ addRequestHeader "X-Api-Key" key req

getJSON :: FromJSON a => String -> IO a
getJSON url = do
  req <- authed =<< parseRequest url
  getResponseBody <$> httpJSON req

postJSON_ :: ToJSON a => String -> a -> IO Bool
postJSON_ url body = do
  req <- authed . setRequestMethod "POST" . setRequestBodyJSON body =<< parseRequest url
  rsp <- httpLbs req
  let status = getResponseStatusCode rsp
  let success = (status >= 200 && status < 300)
  unless success $ log ("post failed: " <> "\n" <> show rsp <> "\nbody:" <> show (getResponseBody rsp))
  pure success

-- === matching + ranking ===

isMatchOf :: Track -> Source -> Bool
isMatchOf t src =
       on T.isInfixOf normalizeText (getArtist t.artist) (T.pack src.file.filename)
    && on T.isInfixOf normalizeText (getAlbum t.album)   (T.pack $ W.takeDirectory src.file.filename)
    && on T.isInfixOf normalizeText (getTitle t.title)   (T.pack $ W.takeFileName src.file.filename)
    && and (src.file.attributes >>= (.aDuration) <&> (== t.duration))

estimateKbps :: Integer -> Duration -> Int
estimateKbps bytes duration =
  let durSec = getDuration duration in
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
    putStrLn $ "Searching for " <> show t

    sid <- UUID.toString <$> UUIDv4.nextRandom
    let sidPath    = urlEncode sid
        searchUrl  = apiV0 <> "/searches"
        searchText = show t.artist <> " - " <> show t.title
    success <- postJSON_ searchUrl $ object
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

    if not success then pure [] else do
      results <- pollJSON (searchUrl <> "/" <> sidPath <> "/responses") 6 10000000
      let filtered = filter (isMatchOf t) . join $ expand <$> results

      putStrLn $ show (length results) <> " results"
      log results
      putStrLn $ show (length filtered) <> " after filtering"
      log filtered

      pure . filter (isMatchOf t) . join $ expand <$> results
  where
    -- simple poller: try n times, sleep d µs between, return first successful parse
    pollJSON :: String -> Int -> Int -> IO [SearchResponse]
    pollJSON url tries delayUs = go tries
      where
        go 0 = pure []
        go n = threadDelay delayUs >> getJSON url >>= \case
          xs@(_:_) -> pure xs
          _        -> go (n-1)

-- download the first viable result; timeout each candidate; move on if stalled
download :: FilePath -> [Source] -> IO (Maybe FilePath)
download dest srcs = runMaybeT . asum $ MaybeT . downloadOne 30 dest <$> srcs

downloadOne :: Int -> FilePath -> Source -> IO (Maybe FilePath)
downloadOne seconds downloadsDir src = do
    log ("Trying to download",src)
    success <- postJSON_ downloadsUrl $ EnqueueRequest src.file
    if not success
      then pure Nothing
      else loop =<< Just . addUTCTime (fromIntegral seconds) <$> getCurrentTime
  where
    downloadsUrl = apiV0 <> "/transfers/downloads/" <> urlEncode src.username

    -- 'deadline' is active when we're NOT in progress. When we observe InProgress, we set it to Nothing.
    loop :: Maybe UTCTime -> IO (Maybe FilePath)
    loop mDeadline = do
      now <- getCurrentTime
      v   <- getJSON downloadsUrl
      log ("loop got json",v)
      let ds = fromRight [] $ downloadsFromEnv v
          timedOut = maybe False (now >) mDeadline

      case find relevant ds of
        Just d@Download{status = Just st, speed, bytesDone}
          | any (`isInfixOf` st) ["Failed","Rejected"] -> log ("Failed",d)   $> Nothing
          | "Complete"   `isPrefixOf` st               -> log ("Complete",d) $> Just (destination d)
          | "InProgress" `isInfixOf`  st -> do
              log ("Waiting",d)
              threadDelay 300000
              loop Nothing
        dl | timedOut  -> Nothing <$ (putStrLn "Timed out, trying next..." >> for_ dl cancelDownload)
           | otherwise -> do
              let newDeadline = mDeadline <|> Just (addUTCTime (fromIntegral seconds) now)
              threadDelay 300000
              loop newDeadline


    downloadsFromEnv :: Value -> Either String [Download]
    downloadsFromEnv = parseEither $ withObject "env" \o -> do
      dirs <- o .:? "directories" .!= []                    -- [Value]
      concat <$> forM dirs (withObject "dir" $ \d -> d .:? "files" .!= [])

    -- We need to decompose the path because we don't know if it's Windows or Posix
    destination :: Download -> FilePath
    destination d = let folder = W.takeFileName . W.takeDirectory $ d.filename
                        file   = W.takeFileName d.filename
                    in downloadsDir </> folder </> file

    relevant :: Download -> Bool
    relevant d = d.filename == src.file.filename && d.size == src.file.size

retag :: Track -> FilePath -> IO ()
retag Track{title=Title title,artist=Artist artist,album=Album album} filePath = do
  putStrLn $ "Retagging: " <> filePath
  threadDelay 10000
  old <- getTrack filePath
  HTagLib.setTags filePath Nothing
     $ HTagLib.titleSetter title
    <> HTagLib.artistSetter artist
    <> HTagLib.albumSetter album
  new <- getTrack filePath
  putStrLn $ "Retagged: " <> filePath
  putStrLn $ "old: " <> show old
  putStrLn $ "new: " <> show new


readPlaylist :: String -> String -> String -> IO (String, [Track])
readPlaylist playlistURL clientId clientSecret = do
    let Just playlistId@(_:_) = takeFileName . uriPath <$> parseURI playlistURL

    spotifyToken <- getSpotifyBearer
    response <- httpJSON
                $ addRequestHeader "authorization" ("Bearer " <> spotifyToken)
                $ setRequestMethod "GET"
                $ parseRequest_ ("https://api.spotify.com/v1/playlists/" <> playlistId <> "/tracks")
    let status = getResponseStatusCode response
    unless (status >= 200 && status < 300) $ error ("getting playlist tracks failed: " <> show response)

    pure (playlistId, extractSpotifyTracks . getResponseBody $ response)
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

withSlskd :: String -> String -> FilePath -> [FilePath] -> IO a -> IO a
withSlskd username password downloadsDir shared action = do
    secret <- UUID.toString <$> UUIDv4.nextRandom

    withSystemTempDirectory "slskd-" $ \tmp -> do
      let cfg  = tmp </> "slskd.yml"
          args = concat
            [ ["--config",cfg,"--no-logo"]
            , ["--headless","--http-port","5030","--no-https"]
            , ["--downloads", downloadsDir]
            , if null shared then [] else "--shared":shared
            , ["--slsk-username",username,"--slsk-password",password]
            ]
      Yaml.encodeFile cfg $ genConfig secret
      setFileMode cfg 0o600
      bracket
        -- Nix flake puts slskd on PATH
        (createProcess (proc "slskd" args))
        (\(_,_,_,ph) -> terminateProcess ph >> void (waitForProcess ph))
        (const $ waitForHealth >> setEnv "SLSKD_API_KEY" secret >> action)

data Cmd = Cmd { playlist :: String, clientId :: String, clientSecret :: String
               , username :: String, password :: String
               , downloads :: FilePath, shared :: [FilePath]
               , debug :: Bool }
  deriving (Show, Generic)
instance ParseRecord Cmd where parseRecord = parseRecordWithModifiers lispCaseModifiers

getTrack :: FilePath -> IO Track
getTrack path = HTagLib.getTags path
              $ Track
            <$> fmap Title    HTagLib.titleGetter
            <*> fmap Artist   HTagLib.artistGetter
            <*> fmap Album    HTagLib.albumGetter
            <*> fmap Duration HTagLib.durationGetter

getMissing :: FilePath -> [Track] -> IO ([Track], [Track])
getMissing downloadsDir playlist = do
  existingTracks <- mapM getTrack . mapMaybe (\case {DirTree.File _ file -> Just file; _ -> Nothing})
                  . DirTree.flattenDir . (.dirTree) =<< DirTree.build downloadsDir
  putStrLn $ "local tracks: " <> show (length existingTracks)

  let (have,missing) = partition (`elem` existingTracks) playlist
  putStrLn $ " have tracks: " <> show (length have)
  putStrLn $ " miss tracks: " <> show (length missing)

  pure (have, missing)

expandTilde :: FilePath -> IO FilePath
expandTilde ('~':path) = getHomeDirectory <&> (</> path)
expandTilde path       = pure path

createM3uPlaylist :: FilePath -> String -> [Track] -> IO ()
createM3uPlaylist downloadsDir playlistId tracks = do
  files <- mapMaybe (\case {DirTree.File _ file -> Just file; _ -> Nothing})
         . DirTree.flattenDir . (.dirTree) <$> DirTree.build downloadsDir
  trackFiles <- filterM (\f -> (`elem` tracks) <$> getTrack f) files

  let m3uPath = downloadsDir </> playlistId <> ".m3u"
      m3uContent = "#EXTM3U\n" <> concatMap formatTrack trackFiles

  writeFile m3uPath m3uContent
  putStrLn $ "Created playlist: " <> m3uPath <> " with " <> show (length trackFiles) <> " tracks"
  where
    formatTrack :: FilePath -> String
    formatTrack filePath =
      let relPath = makeRelativePath downloadsDir filePath
      in relPath <> "\n"

    makeRelativePath :: FilePath -> FilePath -> FilePath
    makeRelativePath base path =
      if base `isPrefixOf` path
        then drop (length base + 1) path
        else takeFileName path

main :: IO ()
main = do
  Cmd{..} <- getRecord "playlist-sync"

  setEnv "DEBUG" $ show debug

  (playlistId, playlistTracks) <- readPlaylist playlist clientId clientSecret
  putStrLn $ " list tracks: " <> show (length playlistTracks)

  downloadsDir <- downloads & \case
    ('~':path) -> getHomeDirectory <&> (</> path)
    _          -> pure downloads
  (_, missing) <- getMissing downloadsDir playlistTracks

  withSlskd username password downloads shared $
    forM_ missing $ \track ->
      search track >>= download downloadsDir . prioritize track
      >>= maybe (putStrLn $ "Couldn't download " <> show track) (retag track)

  (finalHave, finalMissing) <- getMissing downloadsDir playlistTracks
  createM3uPlaylist downloadsDir playlistId finalHave

  putStrLn "FINISHED! Still missing:"
  mapM_ (putStrLn . show) finalMissing
  
