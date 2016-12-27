import Control.Exception (assert)
import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import qualified Data.HashSet as S
import Utils

type Key = Int

type Point = (Int, Int)

type Generation = (Int, S.HashSet Point)

as_binary :: Int -> String
as_binary n = showIntAtBase 2 intToDigit n ""

is_open :: Key -> Point -> Bool
is_open k (x, y)
    | x < 0 || y < 0 = False
    | otherwise      =
        let n = x*x + 3*x + 2*x*y + y + y*y
            s = as_binary (n + k)
            c = length $ filter (== '1') s
        in even c

draw :: Key -> Point -> Point -> [String]
draw k (x, y) (x', y') = map (\r ->
    map (\c -> if is_open k (c, r) then '.' else '#') [x..x']) [y..y']

step :: Key -> Point -> S.HashSet Point
step key (x, y) = S.fromList $ filter (is_open key) [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

generations :: Key -> Point -> [Generation]
generations key start = scanl (\(_, ps) n -> (n, S.unions (map (step key) (S.toList ps)))) (0, S.singleton start) [1..]

step_count :: Key -> Point -> Point -> Int
step_count key start dest =
    let gen = head $ dropWhile (\g -> not (S.member dest (snd g))) (generations key start)
    in fst gen

part_one :: Int -> Int
part_one input = step_count input (1, 1) (31, 39)

point_count :: Key -> Point -> Int -> Int
point_count key start steps = S.size $ S.unions $ map snd (take (steps + 1) (generations key start))

part_two :: Int -> Int
part_two input = point_count input (1, 1) 50

test_grid = ".#.####.##\n\
            \..#..#...#\n\
            \#....##...\n\
            \###.#.###.\n\
            \.##..#..#.\n\
            \..##....#.\n\
            \#...##.###"

main = do
    let input = 1364

    print $ assert (is_open 10 (0, 0)) "0 0"
    print $ assert (is_open 10 (0, 1)) "0 1"
    print $ assert (is_open 10 (1, 1)) "1 1"
    print $ assert (not (is_open 10 (2, 1))) "2 1"

    let r = draw 10 (0, 0) (9, 6)
    prints r
    print $ assert ((lines test_grid) == r) "draw/open passed"

    prints $ draw input (0, 0) (40, 50)

    let r = step_count 10 (1, 1) (7, 4)
    print r
    print $ assert (11 == r) "example one passed!"

    let r = part_one input
    print r
    print $ assert (86 == r) "part one passed!"

    let r = point_count 10 (1, 1) 0
    print r
    print $ assert (1 == r) "test one passed!"

    let r = point_count 10 (1, 1) 1
    print r
    print $ assert (3 == r) "test two passed!"

    let r = point_count 10 (1, 1) 2
    print r
    print $ assert (5 == r) "test three passed!"

    let r = part_two input
    print r
    print $ assert (127 == r) "part two passed!"
