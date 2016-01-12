{-# LANGUAGE OverloadedStrings #-}

module Day12 where

import Data.Aeson
import Data.Maybe
import Data.Scientific
import Data.ByteString.Lazy.Char8 (pack)

decodeJson :: String -> Value
decodeJson = fromJust . decode . pack

addNumbers :: Value -> Int
addNumbers = \case
  Number n -> fromJust $ toBoundedInteger n
  Array  a -> sum $ addNumbers <$> a
  Object o -> sum $ addNumbers <$> o
  _        -> 0

addNonRedNumbers :: Value -> Int
addNonRedNumbers = \case
  Number n -> fromJust $ toBoundedInteger n
  Array  a -> sum $ addNonRedNumbers <$> a
  Object o
    | any valueIsRed o -> 0
    | otherwise -> sum $ addNonRedNumbers <$> o
  _ -> 0

valueIsRed :: Value -> Bool
valueIsRed = \case
  String t -> t == "red"
  _        -> False