import Control.Exception (assert)

part_one :: String -> Int
part_one input = length input

--part_two :: String -> Int
--part_two input = length input

main = do
    input <- readFile "day_input.txt"
    let r = part_one input
    print r
    print $ assert (0 == r) "part one passed!"
--    let r = part_two input
--    print r
--    print $ assert (0 == r) "part two passed!"
