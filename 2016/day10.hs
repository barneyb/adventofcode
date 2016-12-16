import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Utils

valuere :: String
valuere = "value ([0-9]+) goes to bot ([0-9]+)"

passre :: String
passre = "bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (bot|output) ([0-9]+)"

part_one :: String -> Int -> Int -> Int
part_one input l h = -1

--part_two :: String -> Int
--part_two input = length input

test_input =
    "value 5 goes to bot 2\n\
    \bot 2 gives low to bot 1 and high to bot 0\n\
    \value 3 goes to bot 1\n\
    \bot 1 gives low to output 1 and high to bot 0\n\
    \bot 0 gives low to output 2 and high to output 0\n\
    \value 2 goes to bot 2"

main = do
    input <- readFile "day10_input.txt"

    let r = part_one test_input 2 5
    print r
    print $ assert (2 == r) "test one passed!"

    let r = part_one test_input 2 3
    print r
    print $ assert (1 == r) "test two passed!"

    let r = part_one input 17 61
    print r
    print $ assert (0 == r) "part one passed!"

--    let r = part_two input
--    print r
--    print $ assert (0 == r) "part two passed!"
