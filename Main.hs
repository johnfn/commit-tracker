{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import System.IO
import GHC.Generics
import Network.HTTP.Conduit
import Control.Monad
import Data.List
import Data.List.Split
import System.Environment
import Data.Typeable
import Data.Data
import Data.Time.Calendar
import Data.Time.Format
import System.Locale
import Data.Maybe
import Data.Aeson.Generic
import qualified Data.Text as Text
import qualified Data.Map as Map
import qualified Data.HashMap.Lazy as HM
import Debug.Trace

import qualified Data.ByteString.Lazy as L

data Author = Author {
  name :: String,
  email :: String,
  date :: String
} deriving (Data, Typeable, Show)

data CommitInfo = CommitInfo {
  author :: Author
} deriving (Data, Typeable, Show)

data Commit = Commit {
  commit :: CommitInfo
} deriving (Data, Typeable, Show)

data Repo = Repo {
  full_name :: String
} deriving (Data, Typeable, Show)

userName :: IO String
userName = do
  args <- getArgs
  return (if (length args > 0) then head args else "johnfn")

loadPage :: String -> IO L.ByteString
loadPage url = do
    withManager $ \manager -> do
                      res <- httpLbs req manager
                      return (responseBody res)
  where
    req0 = fromJust $ parseUrl url
    req = req0 { requestHeaders = [("User-Agent", "johnfn")] }

-- TODO: investigate what it means to cast to Maybe Day and how we can cast to many different types. seemz cool.

-- String is dumb, but Days aren't hashable (??????)
getDate :: Commit -> Day
getDate = fromJust . (parseTime defaultTimeLocale "%Y-%m-%d") . (takeWhile (/='T')) . date . author . commit

-- my candidate for sexiest line of haskell
-- this would be like 15 lines of javascript
buildMap :: [String] -> HM.HashMap String Int
buildMap days = HM.fromListWith (+) $ zip days $ repeat 1

repoNames :: String -> IO [String]
repoNames username = do
  repos <- loadPage $ "https://api.github.com/users/" ++ username ++ "/repos?q=&per_page=100" -- someday, i might have to deal with having more than 100 repos.
  return $ map full_name $ fromJust (decode repos :: Maybe [Repo])

loadCommitDates :: String -> String -> IO [Day]
loadCommitDates username reponame = do
  page <- loadPage $ "https://api.github.com/repos/" ++ username ++ "/" ++ reponame ++ "/commits"
  return $ map getDate $ fromJust (decode page :: Maybe [Commit])

arrToTuple :: [a] -> (a, a)
arrToTuple [a, b] = (a, b)
arrToTuple _ = error "arrToTuple on something weird"

test a b = putStrLn (a ++ "," ++ b)

main :: IO ()
main = do
  dates <- loadCommitDates "johnfn" "Fathom"
  putStrLn $ show dates

  repos <- repoNames "johnfn" >>= return . (map $ arrToTuple . (splitOn "/")) . take 5

  sequence $ map (uncurry test) repos

  return ()

  {-
  uname <- userName
  r <- repoNames uname

  allData <- map 
  -}
  
  {-
  massive <- loadPage $ "https://api.github.com/repos/" ++ u ++ "/commit-tracker/commits"
  let json = fromJust (decode massive :: Maybe [Commit])
  let dates = map (show . getDate) json
  let frequencies = buildMap dates
  let sortedKeys = sortBy (\(d1, v1) (d2, v2) -> compare d1 d2) $ HM.toList frequencies

  putStrLn $ show $ sortedKeys

  sequence $ map (\(d, v) -> putStrLn $ d ++ " " ++ (replicate v '+')) sortedKeys

  return ()
  -}
