{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day12 where

import Data.Aeson
import Data.Maybe
import qualified Data.Vector as V
import Data.Scientific
import Data.ByteString.Lazy.Char8 (pack)
import qualified Data.HashMap.Strict as HM

findAndAddNumbers :: String -> Int
findAndAddNumbers = addNumbers . decodeJson

findAndAddNonRedNumbers :: String -> Int
findAndAddNonRedNumbers = addNonRedNumbers . decodeJson

decodeJson :: String -> Value
decodeJson = fromJust . decode . pack

addNumbers :: Value -> Int
addNumbers (Number n) = fromJust $ toBoundedInteger n
addNumbers (Array a) = sum . map addNumbers $ V.toList a
addNumbers (Object o) = sum . map addNumbers $ HM.elems o
addNumbers _ = 0

addNonRedNumbers :: Value -> Int
addNonRedNumbers (Number n) = fromJust $ toBoundedInteger n
addNonRedNumbers (Array a) = sum . map addNonRedNumbers $ V.toList a
addNonRedNumbers (Object (HM.elems -> vs))
  | any valueIsRed vs = 0
  | otherwise = sum $ map addNonRedNumbers vs
addNonRedNumbers _ = 0

valueIsRed :: Value -> Bool
valueIsRed (String t) | t == "red" = True
valueIsRed _ = False