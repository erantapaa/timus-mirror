{-# LANGUAGE OverloadedStrings #-}

module TimusHttp (
  fetchUrl
  , fetchUrl'
  , fetchProblem
  , fetchImage
  , fetchProblemMap
  , fetchThreads
  , fetchProblemThreadsFrom
  , fetchNewestThreads
  , atomicWrite
  , generateFile
  , logStr
  , logStrLn
) where

import Control.Lens hiding (element)
import Network.Wreq
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Filesystem.Path.CurrentOS as FP
import System.Directory (doesFileExist,renameFile)
import System.IO (hFlush,stdout,Handle)
import System.IO.Temp
import qualified Data.ByteString.Lazy.Char8 as LBS

fetchUrl' url = fetchUrl url defaults

-- | return the body of a page as a LBS
fetchUrl url opts = do
  r <- getWith opts url
  let status = r ^. responseStatus . statusCode
  if status == 200
      then return $ r ^. responseBody
      else error $ "http request for " ++ url ++ " failed: " ++ show status

-- | fetch a problem page
fetchProblem problemId = do
  let url = "http://acm.timus.ru/problem.aspx"
      opts = defaults & param "space" .~ ["1"] & param "num" .~ [T.pack $ show problemId]
  fetchUrl url opts

-- | fetch an image
fetchImage imageId = do
  let url = "http://acm.timus.ru/image/get.aspx/" ++ imageId
      opts = defaults
  fetchUrl url opts

-- | fetch the solved problem map for a specific author
fetchProblemMap authorId = do
  let url = "http://acm.timus.ru/author.aspx"
      opts = defaults & param "id" .~ [authorId]
  fetchUrl url opts

-- | fetch the discussion threads for a problem
fetchThreads problemId = do
  let url = "http://acm.timus.ru/forum"
      opts = defaults & param "space" .~ ["1"] & param "num" .~ [problemId]  & param "style" .~ ["tree"]
  fetchUrl url opts

fetchProblemThreadsFrom problemId from = do
  let url = "http://acm.timus.ru/forum"
      opts0 = defaults & param "space" .~ ["1"] & param "num" .~ [problemId]  & param "style" .~ ["tree"]
      opts = if T.null from then opts0 else opts0 & param "from" .~ [from]
  fetchUrl url opts

-- | fetch the newest threads beginning from a timestamp; use "" to start from the current time
fetchNewestThreads from = do
  let url = "http://acm.timus.ru/forum"
      opts0 = defaults & param "style" .~ ["tree"]
      opts = if T.null from then opts0 else opts0 & param "from" .~ [from]
  fetchUrl url opts

-- | Atomically write a file.
atomicWrite :: FP.FilePath -> (Handle -> IO a) -> IO a
atomicWrite path writer = do
  let dir = FP.encodeString $ FP.parent path
  withTempFile dir ".temp-XXXXXXX" $ \tpath thandle -> do
    a <- writer thandle
    renameFile tpath (FP.encodeString path)
    return a

-- | Simple logging functions.
logStr s = do putStr s; hFlush stdout
logStrLn s = do putStrLn s; hFlush stdout

-- | Create a file with an action if it does not exist.
generateFile path action = do
  b <- doesFileExist path
  if b
    then return True
    else do logStr $ "generating " ++ path ++ " - "
            bytes <- action
            atomicWrite (FP.decodeString path) (\h -> LBS.hPutStr h bytes)
            logStrLn "done"
            return True

