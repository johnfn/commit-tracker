{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- TODO: Investigate what it means to cast to Maybe Day and how we can cast to many different types. seemz cool.
-- TODO: Pagination

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

params :: [(String, String)] -> String
params paramList = "?q=" ++ (intercalate "&" $ map showParam paramList)
  where
    showParam (key, val) = key ++ "=" ++ (val)

-- String is dumb, but Days aren't hashable (??????)
getDate :: Commit -> Day
getDate = fromJust . (parseTime defaultTimeLocale "%Y-%m-%d") . (takeWhile (/='T')) . date . author . commit

-- my candidate for sexiest line of haskell
-- this would be like 15 lines of javascript
buildMap :: (Day -> Day) -> [Day] -> Map.Map Day Int
buildMap bucketingScheme days = 
    Map.fromListWith (+) $ zip bucketedDays $ repeat 1
  where
    bucketedDays = map bucketingScheme days

showMap :: Map.Map Day Int -> IO ()
showMap frequencies = do
    sequence_ $ map (\(d, v) -> putStrLn $ (show d) ++ " " ++ replicate v '+') sortedKeys
  where
    sortedKeys :: [(Day, Int)] = sortBy (\k1 k2 -> compare (fst k1) (fst k2)) $ Map.toList frequencies

repoNames :: String -> IO [(String, String)]
repoNames username = do
    repoJSON <- loadPage $ "https://api.github.com/users/" ++ username ++ "/repos" ++ params [("per_page", "100")] -- someday, i might have to deal with having more than 100 repos.
    let repos :: [Repo] = fromJust (decode repoJSON :: Maybe [Repo])

    return $ map grabName $ repos 
  where 
    grabName :: Repo -> (String, String) = arrToTuple . splitOn "/" . full_name

noquotes :: [a] -> [a]
noquotes = tail . init

-- NOTE : repoOwner may be different than the current user...
loadCommitObj :: Maybe String -> String -> String -> IO [Commit]
loadCommitObj since repoOwner reponame = do
    page <- loadPage $ "https://api.github.com/repos/" ++ repoOwner ++ "/" ++ reponame ++ "/commits" ++ paramList
    return $ fromJust (decode page :: Maybe [Commit])
  where
    paramList = params $ [("per_page", "100"), ("author", repoOwner)] ++ (fromMaybe [] (fmap (\x -> [("until", x)]) since))

loadCommitDates :: Maybe String -> String -> String -> IO [Day]
loadCommitDates since repoOwner reponame = 
  loadCommitObj since repoOwner reponame >>= return . (map getDate)

loadAllCommits :: String -> String -> IO [Day]
loadAllCommits repoOwner reponame = do
    go [] Nothing
  where
    go acc oldestDay = do
      commitObjs :: [Commit] <- loadCommitObj oldestDay repoOwner reponame
      let commits :: [Day] = map getDate commitObjs

      if (length commits) == 0 then
        return acc
      else do
        go (acc ++ commits) (Just $ (noquotes. show . date . author . commit) (last commitObjs))

arrToTuple :: [a] -> (a, a)
arrToTuple [a, b] = (a, b)
arrToTuple _ = error "arrToTuple on something weird"

main :: IO ()
main = do
  cc <- loadAllCommits "johnfn" "Fathom"
  putStrLn $ show cc
