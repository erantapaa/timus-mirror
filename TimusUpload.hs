{-# LANGUAGE OverloadedStrings #-}

module TimusUpload where

import Network.Wreq
import qualified Data.ByteString.Lazy.Char8 as LBS
import Control.Lens hiding (element)

main = undefined

-- postUrl = "http://requestb.in/wm58tnwm"
submitUrl = "http://acm.timus.ru/submit.aspx?space=1" 

test = do
  let params = [ partText "Action" "submit"
                 , partText "SpaceID" "1"
                 , partText "JudgeID" "163747NOXXX"
                 , partText "Language" "19"
                 , partText "ProblemNum" "1000"
                 , partText "Source" ""
                 , partFile "SourceFile" "1000.hs"
               ]
  r <- post submitUrl params
  let body = r ^. responseBody
  putStrLn $ "Reposonse Status: " ++ show (r ^. responseStatus)
  LBS.writeFile "response.html" body

-- form action="submit.aspx?space=1 enctype="multipart/form-data" 

-- input name=JudgeID
-- select name=Language
--    Haskell == 19
-- input ProblemNum
-- textarea name=Source
-- input hidden name=Action value=submit
-- input hidden name=SpaceID value=1

