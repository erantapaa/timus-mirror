{-# LANGUAGE NoMonomorphismRestriction #-}

module TimusCommon (
  (|>)
  , firstOr
  , firstMatch
  , (=~)
  , logStr
  , logStrLn
)
where

import Data.Text (Text)
import qualified Data.Text as T

import qualified Text.Regex.TDFA as RE
import qualified Text.Regex.TDFA.Text as RE

import System.IO (hFlush,stdout)

-- | @flip (.)@, fixity is @infixl 9@ (same as for @.@), from F#.
(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) = flip (.)

infixl 9 |>

firstOr :: a -> [a] -> a
firstOr _ (a:_) = a
firstOr a _     = a

-- | function to assist in regular expression matching
firstMatch :: Text -> (Text -> a) -> Text -> Maybe a
firstMatch regex f text =
  case RE.getAllTextSubmatches $ text RE.=~ regex of
    (_:a:_) -> Just $ f a
    _       -> Nothing

-- | Two argument version of firstMatch
firstMatch' :: Text -> (Text -> Text -> a) -> Text -> Maybe a
firstMatch' regex f text =
  case RE.getAllTextSubmatches $ text RE.=~ regex of
    (_:a:b:_) -> Just $ f a b
    _         -> Nothing

-- | Substring predicate for Text strings
isSubstring :: Text -> Text -> Bool
isSubstring s t =
  let (_,found) = T.breakOn s t
  in not $ T.null found

(=~) = (RE.=~)

-- logging

-- | Simple logging functions.
logStr s = do putStr s; hFlush stdout
logStrLn s = do putStrLn s; hFlush stdout

