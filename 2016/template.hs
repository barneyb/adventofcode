import Control.Exception (assert)

part_one :: String -> Int
part_one input = length input

--part_two :: String -> Int
--part_two input = length input

main = do
    input <- readFile "day_input.txt"
    print (part_one input)
    print $ assert (0 == (part_one input)) "part one passed!"
--    print (part_two input)
--    print $ assert (0 == (part_two input)) "part one passed!"
