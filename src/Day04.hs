module Day04 where

import Crypto.Hash.MD5
import Data.ByteString (ByteString)
import Data.ByteString.Base16
import Data.ByteString.Char8 (pack)
import Data.List (find)
import qualified Data.ByteString as BS

findAdventCoin :: String -> Int -> Int
findAdventCoin (pack -> key) zs = i
  where
    Just i = find (makesAdventCoin key prefix) [1..]
    prefix = BS.replicate zs 48 -- '0'

makesAdventCoin :: ByteString -> ByteString -> Int -> Bool
makesAdventCoin key prefix i = prefix `BS.isPrefixOf` encode coin
  where coin = hash $ key `BS.append` pack (show i)