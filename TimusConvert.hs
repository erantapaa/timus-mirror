{-# LANGUAGE OverloadedStrings #-}

module TimusConvert where

import Control.Monad
import TimusParseBoard (Message(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import TimusHttp (generateFile)
import TimusUpdate (addPathDepth,sortMessages,dataRoot, threadFile, mappage)
import TimusParse (allProblemIdsFromHtml)
import TimusParseBoard (parseDate)
import qualified Data.Aeson as J
import Data.Aeson (FromJSON, Value(..), parseJSON, (.:))
import Control.Applicative ((<$>), (<*>), pure)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Maybe (fromMaybe)

-- allProblemIdsallProblemIdsFrom mappage

allProblemIds = allProblemIdsFromHtml mappage

newThreadFile pids = dataRoot <> "new-threads/" <> (T.unpack pids) <> ".json"

convert pids = do
  let inpath = threadFile (T.pack $ show pids)
  bytes <- LBS.readFile inpath
  let Just msgs = J.decode bytes
      _ = msgs :: [OldMessage]
      addDate m = m { dateYMDHMS_ = fromMaybe "???" $ parseDate (date_ m) }
  return $ J.encode $ sortMessages $ addPathDepth $ map addDate $ map unWrap_ msgs

doConvert = do
  allids <- allProblemIds
  forM_ allids $ \pids -> do
    let outpath = newThreadFile (T.pack $ show pids)
    generateFile outpath $ convert pids
    return ()

newtype OldMessage = OldMessage { unWrap_ :: Message }

instance FromJSON OldMessage where
  parseJSON (Object v) = OldMessage <$> (
    M <$> v .: "messageId"
      <*> v .: "parentId"
      <*> v .: "author"
      <*> v .: "body"
      <*> v .: "date"
      <*> v .: "title"
      <*> v .: "problem"
      <*> pure "" -- v .: "dateYMDHMS"
      <*> pure []
      <*> pure 0
   )

