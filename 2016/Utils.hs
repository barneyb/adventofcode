module Utils
( assert_equal
, freq
, hist
, md5
, nhash
, snhash
, prints
, putStrLns
, regexgrp
, regexgrps
, update
) where

import Crypto.Hash
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Text.Regex.TDFA

assert_equal :: (Eq a, Show a) => a -> a -> String -> IO ()
assert_equal expected actual message = putStrLn ("[Test: " ++ message ++ "]") >>
    if expected == actual
        then putStrLn "pass"
        else putStrLns [ "expected : " ++ (show expected)
                       , "actual   : " ++ (show actual)
                       , "==> FAILURE!"
                       ]

-- taken from 2015's day 4
md5 :: String -> String
md5 s = show (hash (C.pack s) :: Digest MD5)

-- taken from 2015's day 4 (and renamed from chash)
nhash :: String -> Int -> String
nhash salt n = md5 (salt ++ (show n))

-- a "stretched" variant of nhash: `nhash == (snhash 0)`
snhash :: Int -> String -> Int -> String
snhash r salt n = L.foldl' (\h _ -> md5 h) (nhash salt n) [1..r]

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
prints xs = putStrLns (map show xs)

putStrLns :: [String] -> IO ()
putStrLns ss = foldl1 (>>) $ map putStrLn ss

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
