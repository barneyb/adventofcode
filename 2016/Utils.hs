module Utils
( freq
, hist
, md5
, nhash
) where

import Crypto.Hash
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import qualified Data.List as L

-- taken from 2015's day 4
md5 :: String -> String
md5 s = show (hash (C.pack s) :: Digest MD5)

-- taken from 2015's day 4 (and renamed from chash)
nhash :: String -> Int -> String
nhash key n = md5 (key ++ (show n))

hist :: Ord a => [a] -> M.Map a Int
hist = foldl (\m c -> M.insertWith (+) c 1 m) (M.empty)

freq :: Ord a => [a] -> [(a, Int)]
freq xs =
    let h = M.toList $ hist xs
    in L.sortBy (\b a -> compare (snd a) (snd b)) h
