{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Day12 where

import Data.Aeson
import Data.Maybe
import Data.Scientific
import Data.ByteString.Lazy.Char8 (pack)

decodeJson :: String -> Value
decodeJson = fromJust . decode . pack

addNumbers :: Value -> Int
addNumbers (Number n) = fromJust $ toBoundedInteger n
addNumbers (Array a) = sum $ fmap addNumbers a
addNumbers (Object o) = sum $ fmap addNumbers o
addNumbers _ = 0

addNonRedNumbers :: Value -> Int
addNonRedNumbers (Number n) = fromJust $ toBoundedInteger n
addNonRedNumbers (Array a) = sum $ fmap addNonRedNumbers a
addNonRedNumbers (Object o)
  | any valueIsRed o = 0
  | otherwise = sum $ fmap addNonRedNumbers o
addNonRedNumbers _ = 0

valueIsRed :: Value -> Bool
valueIsRed (String t) = t == "red"
valueIsRed _ = False