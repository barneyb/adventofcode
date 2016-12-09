import Control.Exception (assert)
import qualified Data.Char as C
import Text.Regex.TDFA
import Utils

regex :: String
regex = "\\(([0-9]+)x([0-9]+)\\)"

parse :: String -> String
parse str
    | '(' `elem` str  =
        let (prefix, _, suffix, ps) = str =~ regex :: (String, String, String, [String])
            ns = map read ps
            hs = splitAt (ns !! 0) suffix
            mid = concat $ take (ns !! 1) $ repeat (fst hs)
        in prefix ++ mid ++ (parse (snd hs))
    | otherwise       = str

countR :: String -> Int
countR str
    | '(' `elem` str =
        let (prefix, _, suffix, ps) = str =~ regex :: (String, String, String, [String])
            ns = map read ps
            hs = splitAt (ns !! 0) suffix
        in (length prefix) + (ns !! 1) * (countR (fst hs)) + (countR (snd hs))
    | otherwise      = length str

no_spaces :: String -> String
no_spaces = filter (not . C.isSpace)

part_one :: String -> Int
part_one = length . parse . no_spaces

part_two :: String -> Int
part_two = countR . no_spaces

main = do
    input <- readFile "day09_input.txt"

    let r = parse "ADVENT"
    print r
    print $ assert ("ADVENT" == r) "test 1.one passed"

    let r = parse "A(1x5)BC"
    print r
    print $ assert ("ABBBBBC" == r) "test 1.two passed"

    let r = parse "(3x3)XYZ"
    print r
    print $ assert ("XYZXYZXYZ" == r) "test 1.three passed"

    let r = parse "A(2x2)BCD(2x2)EFG"
    print r
    print $ assert ("ABCBCDEFEFG" == r) "test 1.four passed"

    let r = parse "(6x1)(1x3)A"
    print r
    print $ assert ("(1x3)A" == r) "test 1.five passed"

    let r = parse "X(8x2)(3x3)ABCY"
    print r
    print $ assert ("X(3x3)ABC(3x3)ABCY" == r) "test 1.six passed"

    let r = part_two "(3x3)XYZ"
    print r
    print $ assert ((length "XYZXYZXYZ") == r) "test 2.1 passed"

    let r = part_two "X(8x2)(3x3)ABCY"
    print r
    print $ assert ((length "XABCABCABCABCABCABCY") == r) "test 2.2 passed"

    let r = part_two "X(14x2)(8x2)(2x3)ABCY"
    print r
    print $ assert ((length "XABABABCABABABCYABABABCABABABCY") == r) "test 2.beb.1 passed"

    let r = part_two "(27x12)(20x12)(13x14)(7x10)(1x12)A"
    print r
    print $ assert (241920 == r) "test 2.3 passed"

    let r = part_two "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"
    print r
    print $ assert (445 == r) "test 2.4 passed"

    let r = part_one input
    print r
    print $ assert (183269 == r) "part one passed!"

    let r = part_two input
    print r
    print $ assert (11317278863 == r) "part two passed!"
