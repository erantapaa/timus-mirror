{-# LANGUAGE OverloadedStrings #-}

module TimusParseCommon (
  extractText
  , readHtml
  , elementWithClass
  , divWithClass
  , findDiv
  , elementAttribute
  , mkDoc
) where

import Data.Text (Text)
import qualified Data.Text as T

import qualified Data.ByteString.Lazy.Char8 as LBS

import qualified Text.HTML.DOM.Lenient as X
import qualified Text.XML as X hiding (parseLBS)
import Text.XML.Cursor

import qualified Data.Map as M

extractText :: Text -> X.Node -> Text
extractText brText (X.NodeElement e) = goElement e
  where
    goElement (X.Element eName _ eNodes) =
      if eName == "br"
        then brText
        else T.concat $ map (extractText brText) eNodes
extractText brText (X.NodeContent t) = t
extractText _ _               = ""

-- | read a X.Document from a file
readHtml path = do
  bytes <- LBS.readFile path
  return $ X.parseLBS bytes

-- | create a document from a single element
mkDoc elt =
  let prologue = X.Prologue [] Nothing []
      epilogue = []
      doc = X.Document prologue elt epilogue
  in doc

-- selectors:

elementWithClass e c = element e >=> attributeIs "class" c

divWithClass c = elementWithClass "div" c

-- find a div with a class immediately below a cursor
findDiv :: Text -> Cursor -> [ Cursor ]
findDiv cls = ($/ divWithClass cls)

-- | return an attribute for an Element
elementAttribute :: X.Name -> X.Element -> Maybe Text
elementAttribute attr (X.Element _ amap _) = M.lookup attr amap

