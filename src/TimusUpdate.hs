{-# LANGUAGE OverloadedStrings #-}

module TimusUpdate where

import Control.Monad
import TimusHttp
import TimusParse
import TimusParseCommon (readHtml)
import TimusCommon (firstOr)
import TimusParseBoard (Message(..),messagesFromDoc,previousPageFrom)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified MyDOM as X
import qualified Text.XML.Cursor as X
import qualified Data.Aeson as J
import Data.Monoid ((<>),mappend)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.String (fromString)
import qualified Data.Map as M
import Data.List (foldl',sortBy)
import Data.Ord (comparing)

import Data.Char (ord)

import Data.Text.Encoding

dataRoot = "./data/"
problemFile pid   = dataRoot ++ "pages/" ++ show pid ++ ".html"
trimmedFile pid   = dataRoot ++ "trimmed/" ++ show pid ++ ".html"
imageFile imageId = dataRoot ++ "images/" ++ imageId
mappage           = dataRoot ++ "mappage.html"
problems_json     = dataRoot ++ "problems.json"
status_json       = dataRoot ++ "status.json"
threadFile pids   = dataRoot ++ "threads/" ++ (T.unpack pids) ++ ".json"

allProblemsUrl    = "http://acm.timus.ru/problemset.aspx?space=1&page=all" :: String

tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- return all of the messages for a problem
messagesForProblem problemId = do
  let go ms n from = do
        bytes <- fetchProblemThreadsFrom problemId from
        let 
            path = dataRoot ++ "threads/" ++ (T.unpack problemId) ++ "-" ++ show n ++ ".html"
        -- LBS.writeFile (fromString path) bytes
        let doc = X.parseLBS bytes
            msgs = messagesFromDoc problemId doc
            nextFrom = previousPageFrom (X.fromDocument doc)
            count = length msgs
        
        -- T.putStrLn $ "#" <> problemId <> " from: " <> from <> ", messages: " <> (tshow count) <> ", nextFrom: " <> (firstOr "(empty)" nextFrom)
        logStr $ "# " ++ show count ++ " "
        case nextFrom of
          []        -> return ((msgs:ms),n)
          (from':_) -> go (msgs:ms) (n+1) from'
  (mms,n) <- go [] (1::Int) ""
  let flattened = concat mms
      count = length flattened
  logStr $ " pages: " <> show n <> " messages: " <> show count <> " - "
  let allmsgs = concat mms
  if all checkMessage allmsgs
    then logStr " all ok - "
    else logStr " BAD characters - "
  return allmsgs

checkBytes :: LBS.ByteString -> Bool
checkBytes = undefined

checkText :: T.Text -> Bool
checkText txt = all (\c -> ord c < 65500) $ T.unpack txt

-- | check all of the fields for any bad characters
checkMessage :: Message -> Bool
checkMessage m = all checkText [ author_ m, body_ m, title_ m, date_ m ]

-- | Compute the message paths for a group of messages.
computePaths :: [Message] -> M.Map T.Text [T.Text]
computePaths msgs =
  let mmap = M.fromList [ (messageId_ m, m) | m <- msgs ] -- map from msg ids to Messages
      pathMap = M.fromList [ (messageId_ m, path_ m)  | m <- msgs ]
      path_ m = let prefix = case M.lookup (parentId_ m) mmap of
                               Nothing -> []
                               Just m' -> M.findWithDefault [] (messageId_ m') pathMap
                in prefix ++ [messageId_ m]
  in pathMap

-- | Add path and depth information to a list of Messages
addPathDepth :: [Message] -> [Message]
addPathDepth msgs =
  let pathMap = computePaths msgs
      pathFor m = M.findWithDefault [] (messageId_ m) pathMap
  in [ m { depth_ = d, path_ = p } | m <- msgs, let p =pathFor m, let d = length p ]

-- | Sort messages into presentation order.
--   The messages must already have path and depth information added.
sortMessages :: [Message] -> [Message]
sortMessages msgs =
  let threadFor = head . path_
      -- the map of the last response time for each thread
      lastResponseMap = foldl' go M.empty [ (threadFor m, dateYMDHMS_ m) | m <- msgs ]
        where go m (threadId, date) = M.insertWith max threadId date m
      lastResponse m = M.findWithDefault "" (threadFor m) lastResponseMap
      -- first sort (descendingly) by last response time and then by path
      cmp = mappend (flip $ comparing lastResponse) (comparing path_)
  in sortBy cmp msgs
{-
  let pathMap = computePaths msgs
      pathFor m = M.findWithDefault [] (messageId_ m) pathMap
      threadFor = head . pathFor
      lastResponse = foldl' go M.empty sortedPairs
        where go m (threadId, date) = M.insertWith max threadId date m
      augmentedPath m = (Decreasing a, pathFor m)
        where a = M.findWithDefault "" (threadFor m) lastResponse
      -- sorted = sortBy (comparing messageId_) msgs
      sorted = sortBy (comparing augmentedPath) msgs
      depth m = length $ pathFor m
  in sorted
-}

generateProblemThreads problemId = do
  messages <- messagesForProblem problemId
  return $ J.encode $ sortMessages $ addPathDepth $ messages

updateProblemThread problemId = 
    generateFile (threadFile problemId) $ generateProblemThreads problemId

updateAllThreads = do
  problemIds <- allProblemIdsFromHtml mappage
  forM_ problemIds $ \pid -> updateProblemThread (T.pack $ show pid)

updateEverything = do
  let mapUrl = "http://acm.timus.ru/problemset.aspx?space=1&page=1" -- "http://acm.timus.ru/author.aspx?id=163747"
  generateFile mappage $ fetchUrl' mapUrl

  -- update all of the problem pages
  problemIds <- allProblemIdsFromHtml mappage
  forM_ problemIds $ \problemId -> do
    generateFile (problemFile problemId) $ fetchProblem problemId

  -- generate the problems.json file
  let pairs = [ (pid, problemFile pid) | pid <- problemIds ]
  generateFile problems_json $ generateProblemsJson pairs

  -- generate the status.json file
  generateFile status_json $ generateStatusJson mappage

  -- generate the trimmed problem pages
  forM_ problemIds $ \problemId -> do
    generateFile (trimmedFile problemId) $ generateTrimmedPage problemId (problemFile problemId)
  return ()

  -- scan all of the pages for images and download them
  forM_ problemIds $ \pid -> do
    imageIds <- scrapeImages (problemFile pid)
    forM_ imageIds $ \imageId -> do
      let idString= T.unpack imageId
      generateFile (imageFile idString) $ fetchImage idString

