import Control.Exception (assert)
import Utils

data Disc = Disc Int Int Int

is_open :: Int -> Disc -> Bool
is_open t (Disc ot pc op) =
    let p = (t + ot + op) `mod` pc
    in p == 0

parse :: String -> Disc
parse s =
    let ss = regexgrps s "Disc #([0-9]+) has ([0-9]+) positions; at time=0, it is at position ([0-9]+)\\."
        ns = map read ss
    in Disc (ns!!0) (ns!!1) (ns!!2)

part_one :: [Disc] -> Int
part_one ds = head $ dropWhile pred [0..]
    where
        pred :: Int -> Bool
        pred t = any (not . (is_open t)) ds

--part_two :: String -> Int
--part_two input = length input

test_input = "Disc #1 has 5 positions; at time=0, it is at position 4.\n\
             \Disc #2 has 2 positions; at time=0, it is at position 1."

test_discs = map parse (lines test_input)

main = do
    input <- readFile "day15_input.txt"
    let discs = map parse (lines input)

    let r = part_one test_discs
    print r
    print $ assert (5 == r) "example one passed!"

    let r = part_one discs
    print r
    print $ assert (400589 == r) "part one passed!"

--     let r = part_two input
--     print r
--     print $ assert (0 == r) "part two passed!"
