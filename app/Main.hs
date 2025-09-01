{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson (eitherDecode, FromJSON, parseJSON, withObject, (.:))
import qualified Data.ByteString.Lazy as B
import System.Directory (doesFileExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import System.Process (callCommand)
import Control.Concurrent (threadDelay)
import Control.Exception (try, SomeException)
import Data.Time.Clock (getCurrentTime, diffUTCTime)
import Data.Maybe (isJust)

data Track = Track
  { trackName :: String
  , artist :: String
  , album :: String
  , length :: Int
  } deriving (Show)

instance FromJSON Track where
  parseJSON = withObject "Track" $ \v -> Track
    <$> v .: "trackName"
    <*> v .: "artist"
    <*> v .: "album"
    <*> v .: "length"

main :: IO ()
main = do
  -- Fetch and parse playlist JSON
  playlistJson <- simpleHttp "https://api.spotify.com/v1/playlists/{playlist_id}/tracks"
  let tracks = eitherDecode playlistJson :: Either String [Track]
  case tracks of
    Left err -> putStrLn $ "Error parsing JSON: " ++ err
    Right trackList -> do
      -- Check existing files
      files <- listDirectory "path/to/folder"
      let remainingTracks = filter (not . isTrackDownloaded files) trackList
      mapM_ downloadTrack remainingTracks

isTrackDownloaded :: [FilePath] -> Track -> Bool
isTrackDownloaded files track = any (matchesTrack track) files

matchesTrack :: Track -> FilePath -> Bool
matchesTrack track file = 
  let (name, ext) = splitExtension file
  in trackName track `isInfixOf` name && artist track `isInfixOf` name

downloadTrack :: Track -> IO ()
downloadTrack track = do
  let searchCommand = "slskd search " ++ trackName track ++ " " ++ artist track
  putStrLn $ "Searching for: " ++ trackName track
  callCommand searchCommand
  -- Simulate download and tag rewriting
  threadDelay 30000000 -- 30 seconds
  putStrLn $ "Downloaded and tagged: " ++ trackName track
