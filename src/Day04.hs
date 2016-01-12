module Day04 where

import Data.List (find)
import Data.ByteString.Char8 (pack)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Crypto.Hash.MD5
import Data.ByteString.Base16

findAdventCoin :: String -> Int -> Int
findAdventCoin key zs = i
  where
    (Just i) = find (makesAdventCoin (pack key) prefix) [1..]
    prefix = BS.replicate zs 48 -- '0'

makesAdventCoin :: ByteString -> ByteString -> Int -> Bool
makesAdventCoin key prefix i = prefix `BS.isPrefixOf` encode coin
  where coin = hash $ key `BS.append` (pack $ show i)