{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Conduit
import Control.Monad
import System.Environment
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class (liftIO)

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