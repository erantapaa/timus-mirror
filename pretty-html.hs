import Text.XML hiding (readFile)
import qualified Text.XML as X
import qualified MyDOM as H
import qualified Data.ByteString.Lazy.Char8 as LBS
import System.Environment (getArgs)
import System.IO
import qualified Filesystem.Path as FP
import Data.String

pretty path = do
  doc <- H.readFile path
  let bytes = renderLBS def { rsPretty = True } doc
  LBS.putStr bytes

usage = do
  hPutStrLn stderr $ "Usage: pretty file.xml"

main = do
  args <- getArgs
  case args of
    (arg:_) -> pretty (fromString arg)
    _       -> usage

