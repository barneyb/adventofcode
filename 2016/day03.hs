import Control.Exception (assert)
import qualified Data.List as L

parse_line :: String -> [Int]
parse_line input = L.sort (map read (words input))

parse :: String -> [(Int, Int, Int)]
parse input = map ((\[a, b, c] -> (a, b, c)) . parse_line) (lines input)

part_one :: String -> Int
part_one input = length $ filter (\(a, b, c) -> a + b > c) (parse input)

main = do
    input <- readFile "day03_input.txt"
    print $ assert (1032 == (part_one input)) "part one passed!"
