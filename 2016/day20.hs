import Debug.Trace
import qualified Data.List as L
import Utils

type Range = (Int, Int)

parse :: String -> Range
parse s =
    let ps = regexgrps s "([0-9]+)-([0-9]+)"
        ns = map read ps
    in (ns!!0, ns!!1)

ranges :: String -> [Range]
ranges s = unify $ map parse (lines s)

unify :: [Range] -> [Range]
unify rs = reverse (foldl f [] (L.sort rs))
    where
        f :: [Range] -> Range -> [Range]
        f [] r = [r]
        f rs@((l, h):rst) r@(l', h')
            | l' <= h   = (l, max h h'):rst
            | otherwise = r:rs

holes :: Range -> [Range] -> [Range]
holes (s, e) rs =
    let (hs, e') = foldl f ([], s) rs
        hs' = if e > e' then (hs ++ [(e', e)]) else hs
    in hs'
    where
        f :: ([Range], Int) -> Range -> ([Range], Int)
        f (hs, s) (l, h)
            | l <= s    = (hs, h)
            | otherwise = (hs++[(s, l-1)], h)

part_one :: Range -> String -> Int
part_one r input =
    let hs' = filter (\(l, h) -> l < h) (holes r (ranges input))
        (l, h) = head hs'
    in l + 1

part_two :: Range -> String -> Int
part_two r input = sum $ map (\(l, h) -> h - l) (holes r (ranges input))

test_input = "5-8\n\
             \0-2\n\
             \4-7"

main = do
    input <- readFile "day20_input.txt"

    assert_equal [(0,2),(4,8)] (ranges test_input) "example ranges"
    assert_equal [(2,3),(8,9)] (holes (0, 9) (ranges test_input)) "example holes"

    assert_equal 3 (part_one (0, 9) test_input) "example one"

    assert_equal 19449262 (part_one (0, 4294967295) input) "part one"

    assert_equal 2 (part_two (0, 9) test_input) "example two"

    assert_equal 0 (part_two (0, 4294967295) input) "part two"
