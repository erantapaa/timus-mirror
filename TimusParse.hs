{-# LANGUAGE OverloadedStrings #-}

module TimusParse where

import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)

import qualified MyDOM as X -- for the case-insensitive version of parseLBS
import qualified Text.XML as X hiding (parseLBS)
import Text.XML.Cursor
import qualified Data.Map as M
import qualified Data.Map.Strict as MS

import qualified Data.ByteString.Lazy.Char8 as LBS

import Text.Regex.TDFA
import Text.Regex.TDFA.Text

import Data.Maybe (catMaybes, fromMaybe)

import Control.Arrow ((&&&))

import qualified Data.Aeson as J

-- | @flip (.)@, fixity is @infixl 9@ (same as for @.@), from F#.
(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) = flip (.)

infixl 9 |>

toMaybe [] = Nothing
toMaybe (a:_) = Just a

toList (Just x) = [x]
toList _        = []

-- | function to assist in regular expression matching
firstMatch :: Text -> (Text -> a) -> Text -> Maybe a
firstMatch regex f text = 
  case getAllTextSubmatches $ text =~ regex of
    (_:a:_) -> Just $ f a
    _       -> Nothing

firstMatch' :: Text -> (Text -> Text -> a) -> Text -> Maybe a
firstMatch' regex f text =
  case getAllTextSubmatches $ text =~ regex of
    (_:a:b:_) -> Just $ f a b
    _         -> Nothing

-- | Substring predicate for Text strings
isSubstring :: Text -> Text -> Bool
isSubstring s t =
  let (_,found) = T.breakOn s t
  in not $ T.null found

{-
-- | Determine if a URL points to a problem page
isProblemUrl :: X.Element -> Bool
isProblemUrl e =
  let href = M.findWithDefault "" "href" (X.elementAttributes e)
  in isSubstring "&num=" href
-}

r :: Text -> Text
r = id

-- match "&num=..." in a url
matchNum :: Text -> Maybe Int
matchNum text =
  case getAllTextSubmatches $ text =~ r"&num=([0-9]+)" of
    [] -> Nothing
    (_:a:_) -> Just $ read $ T.unpack a

matchNum' :: Text -> Maybe Int
matchNum' = firstMatch "&num=([0-9]+)" (read . T.unpack)

-- extract all of the text from a Node converting <br> to spaces
extractText :: X.Node -> Text
extractText (X.NodeElement e) = goElement e
  where
    goElement (X.Element eName _ eNodes) =
      if eName == "br"
        then " "
        else T.concat $ map extractText eNodes
extractText (X.NodeContent t) = t
extractText _               = ""

extractText' :: [X.Node] -> Text
extractText' [] = ""
extractText' (n:_) = extractText n

-- extract all of the text from the first cursor
extractText'' [] = ""
extractText'' (c:_) = extractText (node c)

-- | extract all problem ids
extractProblemIds :: Cursor -> [Int]
extractProblemIds = 
  (($// element "a" >=> hasAttribute "href" >=> attribute "href") &| matchNum) |> catMaybes

-- | parse all of the problem ids from a problem map page
allProblemIdsFrom path = do
  doc <- readHtml path
  return $ extractProblemIds (fromDocument doc)

-- | read in a file as HTML; returns a Document
readHtml path = do
  bytes <- LBS.readFile path
  return $ X.parseLBS bytes

-- scrape a problem page
-- scrape the problem map page for problem statuses

elementWithClass e c = ($// (element e >=> attributeIs "class" c))

-- routines to extract attributes from a problem page
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
  let regex = T.concat [ label, "[ :(]*([0-9]+)" ]
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
    tdClass = attribute "class" |> toMaybe |> fromMaybe ""
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

-- generate the trimmed problem page

-- | wrap a list of nodes into a <div>
wrapDiv :: [X.Node] -> X.Element
wrapDiv ns = X.Element "div" M.empty ns

-- | create a Document from a single Element
mkDoc elt =
  let prologue = X.Prologue [] Nothing []
      epilogue = []
      doc = X.Document prologue elt epilogue
  in doc

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

