import Network.HTTP
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
loadCommitHistory username = simpleHttp ("https://api.github.com/repos/" ++ username ++ "/jscity/commits")

main :: IO ()
main = do
  --history <- userName >>= loadCommitHistory

  let req = parseUrl "https://api.github.com/repos/johnfn/jscity/commits"
  withManager $ \manager -> do
                    res <- httpLbs (fromJust req) manager
                    liftIO $ L.putStr $ responseBody res


  -- L.putStrLn history
  -- let name = if (length args > 0) then head args else "johnfn  "