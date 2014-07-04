{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Network.HTTP.Conduit
import Control.Monad
import System.Environment
import Data.Maybe
import Data.List.Split
import Data.List
import Debug.Trace

import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)

data JSON = JVal String | JObj [(String, JSON)] | JArr [JSON] deriving Show

data Tree = Node [String] [Tree] | Leaf deriving Show

getDepths :: [String] -> [(String, Int)]
getDepths list =
    reverse $ foldl go [("", 0)] list
  where
    go wholething@((result, depth):rest) "{" = [("", depth + 1)] ++ wholething
    go wholething@((result, depth):rest) "}" = [("", depth - 1)] ++ wholething
    go ((result, depth):rest) chunk          = [(result ++ chunk, depth)] ++ rest


toTree :: [(String, Int)] -> Tree
toTree [] = Leaf
toTree list =
    Node onLevel $ map toTree (map reduceDepths notOnLevel)
  where
    reduceDepths l = map (\(result, depth) -> (result, depth - 1)) l
    isThisLevel (str, depth) = depth == 0
    onLevel :: [String] = map fst $ filter isThisLevel list
    notOnLevel = splitWhen isThisLevel list

{-
toNestedList :: String -> Tree
toNestedList str =
    if length parts == 1 then
      Value (head parts)
    else

  where
    parts :: [String] = split (oneOf "{}") str
    depths :: [(String, Int)] =
-}

parseJSON :: String -> Tree
parseJSON str = toTree $ getDepths $ split (oneOf "{}") str


-- parseJSON :: String -> JSON

{-
instance FromJSON Commits where
  parseJSON (Object o) = do
    commits <- mapM parseJSON . filter (\(Object ref) -> True) =<< o .: "references"
    commits
-}

userName :: IO String
userName = do
  args <- getArgs
  return (if (length args > 0) then head args else "johnfn")


loadCommitHistory :: String -> IO L.ByteString
loadCommitHistory username = do
    withManager $ \manager -> do
                      res <- httpLbs req manager
                      return (responseBody res)
  where
    req0 = fromJust (parseUrl $ "https://api.github.com/repos/" ++ username ++ "/jscity/commits")
    req = req0 { requestHeaders = [("User-Agent", "johnfn")] }



main :: IO ()
main = do
  loadCommitHistory "johnfn" >>= L.putStrLn

  -- L.putStrLn history
  -- let name = if (length args > 0) then head args else "johnfn  "