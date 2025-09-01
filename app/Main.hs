{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}

import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson (eitherDecode, FromJSON, parseJSON, withObject, (.:), (.:?), ToJSON, encode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import System.Directory (doesFileExist, listDirectory)
import qualified Sound.TagLib as TagLib
import System.FilePath ((</>), takeExtension, splitExtension)
import Data.List
import Data.Char (toLower)
import Control.Concurrent.Async (race)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import System.Process (callCommand, readProcess)
import Control.Monad
import System.Exit (ExitCode(..))
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Maybe (isJust)
import Network.HTTP.Simple

data Track = Track
  { trackName :: String
  , artist :: String
  , album :: String
  , duration :: Int
  } deriving (Show)

data SearchResult = SearchResult
  { user :: String }

instance FromJSON SearchResult where
  parseJSON = withObject "SearchResult" $ \v -> SearchResult
    <$> v .: "user"

instance Eq Track where
  track1 == track2 = and
    [ map toLower track1.trackName == map toLower track2.trackName
    , map toLower track1.artist == map toLower track2.artist
    , map toLower track1.album `isPrefixOf` map toLower track2.album
    || map toLower track2.album `isPrefixOf` map toLower track1.album
    , abs (track1.duration - track2.duration) <= 5
    ]

instance FromJSON Track where
  parseJSON = withObject "Track" $ \v -> Track
    <$> v .: "trackName"
    <*> v .: "artist"
    <*> v .: "album"
    <*> v .: "length"

main :: IO ()
main = do
  response <- httpJSON
              $ setRequestMethod "GET"
              $ setRequestPath ( "https://api.spotify.com/v1/playlists/" <> playlistId <> "/tracks")
              $ defaultRequest
  unless (getResponseStatusCode response == 200) $ error "getting playlist tracks failed"
  let trackList = getResponseBody response

  existingTracks <- mapM extractTrackMetadata =<< listDirectory "path/to/folder"
  let remainingTracks = filter (flip notElem existingTracks) trackList
  forM_ remainingTracks $ \track -> (prioritize <$> search track) >>= download >>= retag track

prioritize :: Track -> [SearchResult] -> [SearchResult]
prioritize track = sortBy isBetter . filter (isMatchOf track)

isBetter :: SearchResult -> SearchResult -> Ordering
isBetter = undefined

isMatchOf :: Track -> SearchResult -> Bool
isMatchOf = undefined

extractTrackMetadata :: FilePath -> IO Track
extractTrackMetadata file = do
  Just tagFile <- TagLib.open file
  Just tag <- TagLib.tag tagFile
  title <- TagLib.title tag
  artist <- TagLib.artist tag
  album <- TagLib.album tag
  Just audioProps <- TagLib.audioProperties tagFile
  duration <- fromIntegral <$> TagLib.duration audioProps
  pure Track
    { trackName = title
    , artist = artist
    , album = album
    , duration = duration
    }

baseUrl = "http://localhost:5030/api/v0"

search :: Track -> IO [SearchResult]
search track = do
  let searchID = show track
  let searchRequest = object
        [ "searchText" .= track.trackName
        , "fileLimit" .= (10000 :: Int)
        , "filterResponses" .= True
        , "maximumPeerQueueLength" .= (1000000 :: Int)
        , "minimumPeerUploadSpeed" .= (0 :: Int)
        , "minimumResponseFileCount" .= (1 :: Int)
        , "responseLimit" .= (100 :: Int)
        , "searchTimeout" .= (15000 :: Int)
        ]
  response <- httpJSON
              $ setRequestMethod "POST"
              $ setRequestPath (baseUrl ++ "/searches")
              $ setRequestBodyJSON searchRequest
              $ defaultRequest
  unless (getResponseStatusCode response == 200) $ error "search POST failed"
  threadDelay 5000
  searchResults <- httpJSON
                   $ setRequestMethod "GET"
                   $ setRequestPath (baseUrl ++ "/searches/" ++ searchID ++ "/responses")
                   $ defaultRequest
  unless (getResponseStatusCode response == 200) $ error "couldn't get search results"
  pure $ getResponseBody searchResults


-- Download the first result. If the download has not finished within 30s, try the next result until one succeeds.
download :: [SearchResult] -> IO FilePath
download results = do
  let enqueueRequest = EnqueueRequest { files = map toEnqueueFile results }
  response <- httpJSON
              $ setRequestMethod "POST"
              $ setRequestPath (baseUrl ++ "/transfers/downloads/" ++ head results.user)
              $ setRequestBodyJSON enqueueRequest
              $ defaultRequest
  unless (getResponseStatusCode response == 200) $ error "download POST failed"
  let EnqueueResponse { ok } = getResponseBody response
  if ok
    then return "Download enqueued successfully"
    else error "Failed to enqueue download"
  where
    toEnqueueFile (SearchResult user) = EnqueueFile { filename = user, size = 0 } -- Adjust as needed

retag :: Track -> FilePath -> IO ()
retag track filePath = do
  Just tagFile <- TagLib.open filePath
  Just tag <- TagLib.tag tagFile
  TagLib.setTitle tag track.trackName
  TagLib.setArtist tag track.artist
  TagLib.setAlbum tag track.album
  TagLib.save tagFile
  putStrLn $ "Tags rewritten for: " ++ filePath
