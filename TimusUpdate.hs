{-# LANGUAGE OverloadedStrings #-}

module TimusUpdate where

import Control.Monad
import TimusHttp
import TimusParse
import TimusParseBoard (Message(..),messagesFromDoc,previousPageFrom,firstOr)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified MyDOM as X
import qualified Text.XML.Cursor as X
import qualified Data.Aeson as J
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.String (fromString)

dataRoot = "./"
problemFile pid   = dataRoot ++ "pages/" ++ show pid ++ ".html"
trimmedFile pid   = dataRoot ++ "trimmed/" ++ show pid ++ ".html"
imageFile imageId = dataRoot ++ "images/" ++ imageId
mappage           = dataRoot ++ "mappage.html"
problems_json     = dataRoot ++ "problems.json"
status_json       = dataRoot ++ "status.json"
threadFile pids   = dataRoot ++ "threads/" ++ (T.unpack pids) ++ ".json"

tshow :: Show a => a -> T.Text
tshow = T.pack . show

-- return all of the messages for a problem
messagesForProblem problemId = do
  let go ms n from = do
        bytes <- fetchProblemThreadsFrom problemId from
        let doc = X.parseLBS bytes
            msgs = messagesFromDoc problemId doc
            nextFrom = previousPageFrom (X.fromDocument doc)
            count = length msgs
            path = dataRoot ++ "threads/" ++ (T.unpack problemId) ++ "-" ++ show n ++ ".html"
        -- LBS.writeFile (fromString path) bytes
        -- T.putStrLn $ "#" <> problemId <> " from: " <> from <> ", messages: " <> (tshow count) <> ", nextFrom: " <> (firstOr "(empty)" nextFrom)
        logStr "#"
        case nextFrom of
          []        -> return ((msgs:ms),n)
          (from':_) -> go (msgs:ms) (n+1) from'
  (mms,n) <- go [] (1::Int) ""
  let flattened = concat mms
      count = length flattened
  logStr $ " pages: " <> show n <> " messages: " <> show count <> " - "
  return $ concat mms

generateProblemThreads problemId = do
  messages <- messagesForProblem problemId
  return $ J.encode messages

updateProblemThread problemId = 
    generateFile (threadFile problemId) $ generateProblemThreads problemId

updateAllThreads = do
  problemIds <- allProblemIdsFrom mappage
  forM_ problemIds $ \pid -> updateProblemThread (T.pack $ show pid)

updateEverything = do
  let mapUrl = "http://acm.timus.ru/author.aspx?id=163747"
  generateFile mappage $ fetchUrl' mapUrl

  -- update all of the problem pages
  problemIds <- allProblemIdsFrom mappage
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

