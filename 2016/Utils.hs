module Utils
( freq
, hist
, md5
, nhash
, prints
, regexgrp
, regexgrps
, update
) where

import Crypto.Hash
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Text.Regex.TDFA

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

-- a la the one for Seq
update :: Int -> a -> [a] -> [a]
update i x xs = take i xs ++ (x : drop (i + 1) xs)

prints :: Show a => [a] -> IO ()
prints xs = foldl1 (>>) $ map print xs

regexgrp :: String -> String -> String
regexgrp s r = head (regexgrps s r)

regexgrps :: String -> String -> [String]
regexgrps str regex =
    let (_, _, _, ps) = str =~ regex :: (String,String,String,[String])
    in ps

-- python-style reduce which just delegates to foldl
reduce :: (a -> x -> a) -> [x] -> a -> a
-- reduce(function(a, x):a, x[], a):a
reduce func list agg = foldl func agg list

-- python-style scan (which it doesn't actually have) which is implemented via reduce
scan :: (a -> x -> a) -> [x] -> a -> [a]
-- scan(function(a, x):a, x[], a):[a]
scan func list agg =
    let (_, values) = reduce work list (agg, [agg])
        work (a, as) x =
            let a' = func a x
            in (a', as ++ [a'])
    in values
