module Day12 where

import Data.Aeson
import Data.Maybe
import qualified Data.Vector as V
import Data.Scientific
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict as HM

findAndAddNumbers :: String -> Int
findAndAddNumbers = addNumbers . fromJust . decode . pack

addNumbers :: Value -> Int
addNumbers (Number n) = fromJust $ toBoundedInteger n
addNumbers (Object o) = sum . map addNumbers $ HM.elems o
addNumbers (Array a) = sum . map addNumbers $ V.toList a
addNumbers _ = 0