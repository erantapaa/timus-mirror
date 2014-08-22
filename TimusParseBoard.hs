
{-# LANGUAGE OverloadedStrings #-}

module TimusParseBoard where

import Control.Monad
import qualified Data.Text as T
import Data.Text (Text)

import TimusCommon ((|>),firstOr,firstMatch, (=~))
import TimusParseCommon (extractText,readHtml,elementWithClass,divWithClass,findDiv,elementAttribute)

import Text.XML.Cursor

import Data.Maybe (catMaybes, fromMaybe)

import Control.Arrow ((&&&))

import qualified Data.Aeson as J
import Data.Aeson (ToJSON, (.=), object)

import Data.Monoid ((<>))

toList (Just x) = [x]
toList _        = []

matchNum :: Text -> Maybe Int
matchNum = firstMatch "&num=([0-9]+)" (read . T.unpack)

-- | extract all problem ids
extractProblemIds :: Cursor -> [Int]
extractProblemIds = 
  (($// element "a" >=> hasAttribute "href" >=> attribute "href") &| matchNum) |> catMaybes

-- | parse all of the problem ids from a problem map page
allProblemIdsFrom path = do
  doc <- readHtml path
  return $ extractProblemIds (fromDocument doc)

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

-- look for an <a> tag and get the text under it
-- look for a <b> tag and get the text under it
-- look for a <span> tag and get the text under it

mkList x = [x]

-- extract the message id from an HREF
-- example: <a href="javascript:Show('634978560947068030')" id="Img634978560947068030" class="forumimg forumimg-msg_minus forum_text_icon">answer</a>
parseHREF :: Text -> Maybe Text
parseHREF = firstMatch  "Show\\('([0-9]+)'" id

classContainsMessage = checkElement go
  where
    go element = maybe False hasMessage $ elementAttribute "class"  element
    hasMessage :: Text -> Bool
    hasMessage t = t =~ ("message " :: Text)

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

      foo =  mkList . extractText ""   . node -- extract text under an element (<br> -> "")
      foo' = mkList . extractText "\n" . node

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

