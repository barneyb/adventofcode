import Control.Exception (assert)
import Data.Array
import Utils

data Cmd = Rect Int Int | Row Int Int | Col Int Int

type Point = (Int, Int)

type Display = Array Point Bool

box :: Int -> Int -> Bool -> [(Point, Bool)]
box w h v = [ ((x, y), v) | x <- [1..w], y <- [1..h] ]

build :: Int -> Int -> Display
build w h = array ( (1, 1), (w, h) ) (box w h False)

execute :: Display -> Cmd -> Display
execute d (Rect w h) = d//(box w h True)

parse :: String -> Cmd
parse s = Rect 1 1

count_lit :: Display -> Int
count_lit d = 0

draw :: Display -> [String]
draw d =
    let ((_, _), (w, h)) = bounds d
    in take h $ repeat (take w $ repeat '.')

write :: Display -> IO ()
write d = prints $ draw d

do_part_one :: Int -> Int -> [Cmd] -> Int
do_part_one w h cmds = count_lit (foldl execute (build w h) cmds)

part_one :: [Cmd] -> Int
part_one = do_part_one 50 6

--part_two :: String -> Int
--part_two input = length input

main = do
    input <- readFile "day08_input.txt"
    let d = execute (build 7 3) (Rect 3 2)
    write d
    print $ assert (["###....",
                     "###....",
                     "......."] == (draw d)) "step one passed!"
    let d = foldl execute (build 7 3) [(Rect 3 2), (Col 2 1)]
    write d
    print $ assert (["#.#....",
                     "###....",
                     ".#....."] == (draw d)) "step two passed!"
    let d = foldl execute (build 7 3) [(Rect 3 2), (Col 2 1), (Row 1 4)]
    write d
    print $ assert (["....#.#",
                     "###....",
                     ".#....."] == (draw d)) "step three passed!"
    let d = foldl execute (build 7 3) [(Rect 3 2), (Col 2 1), (Row 1 4), (Col 2 1)]
    write d
    print $ assert ([".#..#.#",
                     "#.#....",
                     ".#....."] == (draw d)) "step four passed!"
    let cmds = (map parse (lines input))
    let r = part_one cmds
    print r
    print $ assert (0 == r) "part one passed!"
--    let r = part_two input
--    print r
--    print $ assert (0 == r) "part two passed!"
