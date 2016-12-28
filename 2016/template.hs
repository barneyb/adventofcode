import Control.Exception (assert)
import Utils

part_one :: String -> Int
part_one input = length input

--part_two :: String -> Int
--part_two input = length input

test_input = ""

main = do
    input <- readFile "day_input.txt"

    assert_equal 0 (part_one test_input) "example one"

    assert_equal 0 (part_one input) "part one"

--     assert_equal 0 (part_two input) "part two"
