import Control.Exception (assert)
import qualified Data.List as L
import Utils

type Range = (Int, Int)

parse :: String -> Range
parse s =
    let ps = regexgrps s "([0-9]+)-([0-9]+)"
        ns = map read ps
    in (ns!!0, ns!!1)

ranges :: String -> [Range]
ranges s = L.sort $ map parse (lines s)

part_one :: String -> Int
part_one input =
    let rs = ranges input
        (hs, _) = foldl (\(hs, s) (l, h) -> (hs++[(s, l)], h)) ([], 0) rs
        hs' = filter (\(l, h) -> l + 1 < h) hs
        (l, h) = head hs'
    in l + 1

--part_two :: String -> Int
--part_two input = length input

test_input = "5-8\n\
             \0-2\n\
             \4-7"

main = do
    input <- readFile "day20_input.txt"

    assert_equal 3 (part_one test_input) "example one"

    assert_equal 19449262 (part_one input) "part one"

--     assert_equal 0 (part_two input) "part two"
