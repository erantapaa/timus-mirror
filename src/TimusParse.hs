{-# LANGUAGE OverloadedStrings #-}

module TimusParse where

import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)

import TimusCommon (firstOr, (|>), firstMatch)
import TimusParseCommon (readHtml,extractText,elementWithClass,mkDoc)
import qualified Text.XML as X
import Text.XML.Cursor

import Data.Maybe (catMaybes, fromMaybe)

import Control.Arrow ((&&&))

import qualified Data.Map as M
import qualified Data.Aeson as J

import Data.Monoid ((<>))

matchNum :: Text -> Maybe Int
matchNum = firstMatch "&num=([0-9]+)" (read . T.unpack)

-- extract all of the text from the first cursor
extractText'' [] = ""
extractText'' (c:_) = extractText " " (node c)

-- | extract all problem ids
extractProblemIds :: Cursor -> [Int]
extractProblemIds = 
  (($// element "a" >=> hasAttribute "href" >=> attribute "href") &| matchNum) |> catMaybes

-- | parse all of the problem ids from an problem map page
allProblemIdsFromHtml path = do
  doc <- readHtml path
  return $ extractProblemIds (fromDocument doc)

extractTitle =
  ($// elementWithClass "h2" "problem_title") |> extractText''

extractLimits =
  ($// elementWithClass "div" "problem_limits") |> extractText''

extractProblemSource =
  ($// elementWithClass "div" "problem_source") |> extractText''

extractLinks =
  ($// elementWithClass "div" "problem_links") |> extractText''

matchTime = firstMatch "Time limit: +([0-9]\\.[0-9]+) +second" id

matchMemory = firstMatch "Memory limit: +([0-9]+) +MB" id

matchNumber :: Text -> Text -> Text
matchNumber label text =
  let regex = label <> "[ :(]*([0-9]+)"
  in fromMaybe "???" (firstMatch regex id text)

statDifficulty = matchNumber "Difficulty"
statDiscussion = matchNumber "Discussion"
statAll        = matchNumber "All submissions"
statAccepted   = matchNumber "All accepted submissions"

-- scrape the problem attributes from a problem page
scrapeProblemAttrs :: Int -> X.Document -> [(Text,Text)]
scrapeProblemAttrs problemId doc =
  let cursor = fromDocument doc
      title = extractTitle cursor
      source = extractProblemSource cursor
      limits = extractLimits cursor
      links = extractLinks cursor

      timeLimit   = fromMaybe "???" (matchTime limits)
      memoryLimit = fromMaybe "???" (matchMemory limits)

      diff        = statDifficulty links
      discussion  = statDiscussion links
      submissions = statAll links
      accepted    = statAccepted links
      pairs = [ ("title", title),
                ("time-limit", timeLimit),
                ("memory-limit", memoryLimit),
                ("difficulty", diff),
                ("discussion", discussion),
                ("submissions", submissions),
                ("accepted", accepted),
                ("limits", limits),
                ("source", source),
                ("problemId", T.pack $ show $ problemId)
                ]
  in pairs

-- scrape the submission statuses from a problem map page
-- table class="attempt_list"
-- return a list of pairs: (problemId, class)
extractProblemAttemptStatuses :: Cursor -> [(Int,Text)]
extractProblemAttemptStatuses =
  -- table.attempt_list >> td
  ((($// elementWithClass "table" "attempt_list") &// (element "td")) &| process) |> catMaybes
  where
    process = (aHref &&& tdClass) |> fixup
    tdClass = attribute "class"  |> firstOr ""
    aHref   = (($// element "a" >=> attribute "href") &| matchNum) |> fixup2

    fixup :: (Maybe a, b) -> Maybe (a,b)
    fixup (Nothing, a) = Nothing
    fixup (Just a, b)  = Just (a,b)
    fixup2 :: [Maybe a] -> Maybe a
    fixup2 [] = Nothing
    fixup2 (x:_) = x

scrapeProblemStatuses :: X.Document -> [(Int,Text)]
scrapeProblemStatuses doc =
  let cursor = fromDocument doc
  in extractProblemAttemptStatuses cursor

-- generate the contents of the problems.json
generateProblemsJson problemPathPairs = do
  -- iterate over problem ids
  objs <- forM problemPathPairs $ \(problemId,path) -> do
            doc <- readHtml path
            let pairs = scrapeProblemAttrs problemId doc
            return $ (T.pack $ show problemId, M.fromList pairs)
  return $ J.encode $ M.fromList objs

-- generate the contents of the status.json file from a map page
generateStatusJson mapPath = do
  doc <- readHtml mapPath
  let fixup (a,b) = (T.pack $ show a, b)
  return $ J.encode $ M.fromList $ map fixup $ scrapeProblemStatuses doc

-- | wrap a list of nodes into a <div>
wrapDiv :: [X.Node] -> X.Element
wrapDiv ns = X.Element "div" M.empty ns

extractProblemNodes :: Cursor -> [ X.Node ]
extractProblemNodes =
  ($// (element "div" >=> attributeIs "class" "problem_content")) >=> orSelf followingSibling &| node
  where
    process :: [Cursor] -> X.Element
    process = wrapDiv . map node

-- generate the bytes for a trimmed page
generateTrimmedPage problemId path = do
  doc <- readHtml path
  let nds = extractProblemNodes (fromDocument doc)
      doc' = mkDoc (wrapDiv nds)
      bytes = X.renderLBS X.def doc' -- { rsPretty = True }
  return bytes

matchImageURL :: Text -> Maybe Text
matchImageURL = firstMatch "/image/get.aspx/(.*)" id

-- return the images found in a problem file
extractImages =
  ((($// elementWithClass "div" "problem_content")
    >=> ($// element "img" >=> attribute "src")) &| matchImageURL) |> catMaybes

scrapeImages path = do
  cursor <- fmap fromDocument (readHtml path)
  return $ extractImages cursor

