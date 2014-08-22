{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- Utility script to show how an Axis matches an HTML document.

   Usage:

     runhaskell show-matches file.html | less

   Modify the query function below to customize the axis.
 -}

import Control.Monad
import Text.XML hiding (readFile)
import Text.XML.Cursor
import qualified MyDOM as H
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LT
import System.Environment (getArgs)
import System.IO
import qualified Filesystem.Path as FP
import Data.String

class Textable a where
  -- result should always end with a newline
  toText :: a -> LT.Text

instance Textable Cursor where
  toText = toText' . node

instance Show a => Textable a where
  toText = LT.pack . show

-- modify this query
-- query :: Textable a => Cursor -> [a]
query :: Cursor -> [Cursor]
query = ($// (element "div" >=> attributeIs "class" "thread")) 

first :: [Cursor] -> [Cursor]
first (a:_) = [a]
first _ = []

foo = parent

-- <div class="problem_par_normal">

showResults :: (Cursor -> [Cursor]) -> Document -> IO ()
showResults query doc = do
  let cursors = query (fromDocument doc)
      count = length cursors
  forM_ (zip [(1::Int)..] cursors) $ \(i,c) -> do
    putStrLn $ "Match " ++ show i ++ " / " ++ show count ++ ":"
    putStrLn "==========="
    let t = toText' (node c)
    LT.putStr t 
    putStrLn "==========="
    putStrLn ""
  return ()

toText' :: Node -> LT.Text
toText' (NodeElement e) = 
  let doc = mkDoc e
      txt = renderText def { rsPretty = True } doc
  in LT.dropWhile (=='\n') $ LT.dropWhile (/= '\n') txt
toText' (NodeContent t) = LT.fromStrict t
toText' (NodeInstruction _) = "(Processing Instruction)"
toText' (NodeComment _) = "(Comment)"

-- | create a Document from a single Element
mkDoc elt =
  let prologue = Prologue [] Nothing []
      epilogue = []
      doc = Document prologue elt epilogue
  in doc

pretty path = do
  doc <- H.readFile path
  let bytes = renderLBS def { rsPretty = True } doc
  LBS.putStr bytes

usage = do
  hPutStrLn stderr $ "Usage: pretty file.xml"

main = do
  args <- getArgs
  case args of
    (arg:_) -> do doc <- H.readFile (fromString arg)
                  showResults query doc
    _       -> usage

