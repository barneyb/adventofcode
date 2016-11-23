import Control.Exception (assert)
import qualified Data.List as L

has_vowels :: Int -> String -> Bool
has_vowels n s = n <= length (filter (`elem` "aeiou") s)

has_double :: String -> Bool
has_double s = snd (foldl (\a c -> (c, (snd a) || (c == (fst a)))) (' ', False) s)

has_bad_pair :: String -> Bool
has_bad_pair s = any ((flip L.isInfixOf) s) ["ab", "cd", "pq", "xy"]

nice_count :: String -> Int
nice_count input = length $ filter (\l -> (has_vowels 3 l) && (has_double l) && (not (has_bad_pair l))) (lines input)

has_double_pair :: String -> Bool
has_double_pair [] = False
has_double_pair (_:[]) = False
has_double_pair (_:_:[]) = False
has_double_pair (x:y:xs) = (L.isInfixOf ([x, y]) xs) || (has_double_pair (y:xs))

has_split_double :: String -> Bool
has_split_double [] = False
has_split_double (_:[]) = False
has_split_double (_:_:[]) = False
has_split_double (x:y:z:xs) = (x == z) || (has_split_double (y:z:xs))

nice_count_two :: String -> Int
nice_count_two input = length $ filter (\l -> (has_double_pair l) && (has_split_double l)) (lines input)

main = do
    input <- readFile "day05_input.txt"
    print $ assert (has_vowels 3 "aei") "has_vowels"
    print $ assert (has_vowels 3 "aeiouaeiouaeiou") "has_vowels"
    print $ assert (not (has_vowels 3 "dvszwmarrgswjxmb")) "has_vowels"
    print $ assert (has_double "xx") "has_double"
    print $ assert (has_double "abcdde") "has_double"
    print $ assert (not (has_double "jchzalrnumimnmhp")) "has_double"
    print $ assert (has_bad_pair "haegwjzuvuyypxyu") "has_bad_pair"
    print $ assert (not (has_bad_pair "haegwjzuvuyypyu")) "has_bad_pair"
    print $ assert (258 == (nice_count input)) "part one passed"
    print $ assert (53 == (nice_count_two input)) "part two passed"
