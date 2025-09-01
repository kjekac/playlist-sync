{-# LANGUAGE OverloadedStrings, OverloadedRecordDot #-}

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson (eitherDecode, FromJSON, parseJSON, withObject, (.:))
import qualified Data.ByteString.Lazy as B
import System.Directory (doesFileExist, listDirectory)
import qualified Sound.TagLib as TagLib
import System.FilePath ((</>), takeExtension, splitExtension)
import Data.List (isPrefixOf, isInfixOf, isSuffixOf)
import Data.Char (toLower)
import Control.Concurrent.Async (race)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import System.Process (callCommand)
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Maybe (isJust)

data Track = Track
  { trackName :: String
  , artist :: String
  , album :: String
  , duration :: Int
  } deriving (Show)

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
  Right trackList <- eitherDecode <$> simpleHttp "https://api.spotify.com/v1/playlists/{playlist_id}/tracks"
  existingTracks <- mapM extractTrackMetadata =<< listDirectory "path/to/folder"
  let remainingTracks = filter (flip notElem existingTracks) trackList
  mapM_ downloadTrack remainingTracks

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

downloadTrack :: Track -> FilePath -> IO ()
downloadTrack track = do
  let searchCommand = "slskd search " ++ track.trackName ++ " " ++ track.artist
  putStrLn $ "Searching for: " ++ track.trackName
  result <- race (threadDelay 30000000) (callCommand searchCommand)
  case result of
    Left _ -> putStrLn $ "Download timed out for: " ++ track.trackName
    Right _ -> do
      putStrLn $ "Downloaded: " ++ track.trackName
      rewriteTags track "path/to/downloaded/file" -- Replace with actual file path

rewriteTags :: Track -> FilePath -> IO ()
rewriteTags track filePath = do
  Just tagFile <- TagLib.open filePath
  Just tag <- TagLib.tag tagFile
  TagLib.setTitle tag track.trackName
  TagLib.setArtist tag track.artist
  TagLib.setAlbum tag track.album
  TagLib.save tagFile
  putStrLn $ "Tags rewritten for: " ++ track.trackName
