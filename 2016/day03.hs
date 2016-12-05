import Control.Exception (assert)
import qualified Data.List as L

type Triangle = (Int, Int, Int)

parse_line :: String -> [Int]
parse_line input = map read (words input)

parse :: String -> [Triangle]
parse input = map ((\[a, b, c] -> (a, b, c)) . parse_line) (lines input)

test :: Triangle -> Bool
test (a, b, c) =
    let l = L.sort [a, b, c]
        [a', b', c'] = l
    in a' + b' > c'

tri_count :: [Triangle] -> Int
tri_count ls = length $ filter test ls

part_one :: String -> Int
part_one input = tri_count (parse input)

regroup :: [Triangle] -> [Triangle]
regroup [] = []
regroup ((a1, b1, c1):(a2, b2, c2):(a3, b3, c3):ls) = [(a1, a2, a3), (b1, b2, b3), (c1, c2, c3)] ++ (regroup ls)

part_two :: String -> Int
part_two input = tri_count $ regroup (parse input)

test_input = "101 301 501\n\
             \102 302 502\n\
             \103 303 503\n\
             \201 401 601\n\
             \202 402 602\n\
             \203 403 603"

main = do
    input <- readFile "day03_input.txt"
    print $ assert (1032 == (part_one input)) "part one passed!"
    print $ assert (3 == (part_one test_input)) "test two passed!"
    print $ assert (6 == (part_two test_input)) "test two passed!"
    print $ assert (1838 == (part_two input)) "test two passed!"
