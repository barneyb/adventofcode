import Control.Exception (assert)
import qualified Data.Char as C
import Text.Regex.TDFA
import Utils

parse :: String -> String
parse s
    | '(' `elem` s  =
        let (prefix, _, suffix, ps) = s =~ "\\(([0-9]+)x([0-9]+)\\)" :: (String, String, String, [String])
            ns = map read ps
            hs = splitAt (ns !! 0) suffix
        in prefix ++ (concat $ take (ns !! 1) $ repeat (fst hs)) ++ (parse (snd hs))
    | otherwise     = s

part_one :: String -> Int
part_one input = length $ filter (not . C.isSpace) (parse input)

--part_two :: String -> Int
--part_two input = length input

main = do
    input <- readFile "day09_input.txt"

    let r = parse "ADVENT"
    print r
    print $ assert ("ADVENT" == r) "test one passed"

    let r = parse "A(1x5)BC"
    print r
    print $ assert ("ABBBBBC" == r) "test two passed"

    let r = parse "(3x3)XYZ"
    print r
    print $ assert ("XYZXYZXYZ" == r) "test three passed"

    let r = parse "A(2x2)BCD(2x2)EFG"
    print r
    print $ assert ("ABCBCDEFEFG" == r) "test four passed"

    let r = parse "(6x1)(1x3)A"
    print r
    print $ assert ("(1x3)A" == r) "test  five passed"

    let r = parse "X(8x2)(3x3)ABCY"
    print r
    print $ assert ("X(3x3)ABC(3x3)ABCY" == r) "test  passed"

    let r = part_one input
    print r
    print $ assert (183269 == r) "part one passed!"
--    let r = part_two input
--    print r
--    print $ assert (0 == r) "part two passed!"
