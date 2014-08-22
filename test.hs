{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Text.XML.Cursor
import qualified TimusParse as P
import TimusParseBoard
import TimusHttp
import MyDOM (parseLBS)
import qualified Data.Text as T
import qualified Data.Text.IO as  T
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Monoid ((<>))

doc1 = P.readHtml "/tmp/doc1.html"

test1 = do
  d <- doc1
  return $ extractMessage "(no-problem)" "(no parent)" (fromDocument d)

test2 = do
  d <- P.readHtml "./threads/1000.html"
  return $ messagesFromDoc "(no-problem)" d

test3 = do
  d <- P.readHtml "./threads/1000.html"
  let msgs = messagesFromDoc "(no-problem)" d
  forM_ msgs $ \m -> do T.putStrLn $ formatMessage m
  putStrLn $ "messages: " ++ show (length msgs)

test4= do
  d <- P.readHtml "./threads/1000.html"
  let msgs = messagesFromDoc "(no-problem)" d
      content = T.concat $ map formatMessage msgs
  T.writeFile "out" content

-- get all of the messages for a forum, following the "Previous Page" ink
test5 = do
  let problemId = "1000"
      go from = do bytes <- fetchProblemThreadsFrom problemId from
                   let doc = parseLBS bytes
                       msgs = messagesFromDoc problemId doc
                       nextFrom = previousPageFrom (fromDocument doc)
                   T.putStrLn $ "from: " <> from <> ", messages: " <> T.pack (show $ length msgs)
                   case nextFrom of
                     [] -> return ()
                     (from':_) -> go from'
  go ""

main = undefined

