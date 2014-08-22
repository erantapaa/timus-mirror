
{-# LANGUAGE OverloadedStrings #-}

module TimusParseBoard where

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
import Data.Aeson (ToJSON, (.=), object)

import Data.Monoid ((<>))

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

extractText0 :: Text -> X.Node -> Text
extractText0 brText (X.NodeElement e) = goElement e
  where
    goElement (X.Element eName _ eNodes) =
      if eName == "br"
        then brText
        else T.concat $ map (extractText0 brText) eNodes
extractText0 brText (X.NodeContent t) = t
extractText0 _ _               = ""


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

data Message = M { messageId_ :: Text,
                   parentId_  :: Text,
                   author_    :: Text,
                   body_      :: Text,
                   date_      :: Text,
                   title_     :: Text,
                   problemId_ :: Text
               }
  deriving (Show)

instance ToJSON Message where
  toJSON (M mid pid author body date title problem)
    = object [ "messageId" .= mid,
               "parentId"  .= pid,
               "author"    .= author,
               "body"      .= body,
               "date"      .= date,
               "title"     .= title,
               "problem"   .= problem ]
  

first :: [a] -> [a]
first (a:_) = [a]
first _ = []

toElement :: X.Node -> Maybe X.Element
toElement (X.NodeElement e) = Just e
toElement _ = Nothing

-- look for an <a> tag and get the text under it
-- look for a <b> tag and get the text under it
-- look for a <span> tag and get the text under it

-- return the text under the first child element of a given name, or ""
textInsideFirstChild :: X.Name -> Cursor -> Text
textInsideFirstChild tag c = extractText'' (c $/ element tag) 

mkList x = [x]

firstOr :: a -> [a] -> a
firstOr _ (a:_) = a
firstOr a _     = a

(&) a f = f a

-- extract the message id from an HREF
-- example: <a href="javascript:Show('634978560947068030')" id="Img634978560947068030" class="forumimg forumimg-msg_minus forum_text_icon">answer</a>
parseHREF :: Text -> Maybe Text
parseHREF = firstMatch  "Show\\('([0-9]+)'" id

classContainsMessage = checkElement go
  where
    go (X.Element _ attrs _) = maybe False hasMessage $ M.lookup "class" attrs
    hasMessage :: Text -> Bool
    hasMessage t = t =~ r "message "

elementWithClass e c = element e >=> attributeIs "class" c

divWithClass c = elementWithClass "div" c

-- find a div with a class immediately below a cursor
findDiv :: Text -> Cursor -> [ Cursor ]
findDiv cls = ($/ divWithClass cls)

-- cursor points to a forum_shift_right div
extractMessage problemId parentid cursor = 
  let boards = findDiv "board" cursor
      atags  = boards >>= ($/ element "a")
      title  = firstOr "" $ atags >>= foo
      msgid  = firstOr "" $ atags >>= (attribute "href" >=> (toList . parseHREF))
      date   = firstOr "" $ boards >>= ($/ element "span" >=> foo)
      author = firstOr "" $ boards >>= ($/ element "b" >=> foo)
      hidden = boards >>= (followingSibling >=> elementWithClass "div" "hidden_message")
      mbody  = firstOr "" $ hidden >>= ($/ element "div" >=> classContainsMessage >=> foo')

      foo =  mkList . extractText0 ""   . node -- extract text under an element (<br> -> "")
      foo' = mkList . extractText0 "\n" . node

      message = M msgid parentid author mbody date title problemId
  in if null boards
       then Nothing
       else Just message

-- cursor must point to a forum_shift_right div
extractAllMessages problemId parentid cursor =
  case extractMessage problemId parentid cursor of
    Nothing -> []
    Just m  -> let boards = findDiv "board" cursor
                   responses = boards >>= (followingSibling >=> elementWithClass "div" "forum_shift_right")
               in m : concatMap (extractAllMessages problemId (messageId_ m)) responses

-- extract all message from a Document
messagesFromDoc problemId doc =
  let allThreads = (fromDocument doc) $// (elementWithClass "div" "hidden_thread")
      roots = allThreads >>= ($/ divWithClass "thread") >>= ($/ divWithClass "forum_shift_right")
  in concatMap (extractAllMessages problemId "(no parent)") roots

previousPageFrom :: Cursor -> [Text]
previousPageFrom = ($// divWithClass "afterpages board")
                     >=> ($/ element "a" >=> attribute "href" >=> (toList . parseFromEquals) )

-- parse the "&from=..." parameter from a URL
parseFromEquals :: Text -> Maybe Text
parseFromEquals = firstMatch "&from=([0-9]+)" id

-- format a message yaml-style
formatMessage :: Message -> Text
formatMessage m =
  T.unlines [
    "- messageId: " <> messageId_ m
  , "  parentId:  " <> parentId_ m
  , "  title:     " <> title_ m
  , "  author:    " <> author_ m
  , "  date:      " <> date_ m
  , "  body:      " <> summary (body_ m)
   ]
  where
    summary t = s <> dots <> " " <> stats
      where
        lines = T.splitOn "\n" t
        lcount = length lines
        ccount = T.length t
        firstline = firstOr "" lines
        s = T.take 30 firstline
        dots = if T.length firstline > 30 || lcount > 1 then " ..." else ""
        fmt n label = T.pack (show n) <> " " <> label <> (if n == 1 then "" else "s")
        stats = "(" <> fmt ccount "char" <> ", " <> fmt lcount "line" <> ")"

