import Control.Exception (assert)
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Utils

part_one :: String -> String
part_one input = map (fst . head . freq) (L.transpose (lines input))

--part_two :: String -> String
--part_two input = show $ length input

test_input = "eedadn\n\
             \drvtee\n\
             \eandsr\n\
             \raavrd\n\
             \atevrs\n\
             \tsrnev\n\
             \sdttsa\n\
             \rasrtv\n\
             \nssdts\n\
             \ntnada\n\
             \svetve\n\
             \tesnvt\n\
             \vntsnd\n\
             \vrdear\n\
             \dvrsen\n\
             \enarar"

main = do
    input <- readFile "day06_input.txt"
    let r = part_one test_input
    print r
    print $ assert ("easter" == r) "part one passed!"
    let r = part_one input
    print r
    print $ assert ("qtbjqiuq" == r) "part one passed!"
--    let r = part_two input
--    print r
--    print $ assert ("" == r) "part two passed!"
