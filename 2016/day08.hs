import Control.Exception (assert)
import Data.Array
import qualified Data.List as L
import qualified Data.Text as T
import Utils

data Cmd = Rect Int Int | Row Int Int | Col Int Int deriving (Eq, Show)

type Point = (Int, Int)

type Pixel = (Point, Bool)

type Display = Array Point Bool

box :: Int -> Int -> Bool -> [Pixel]
box w h v = [ ((x, y), v) | x <- [0..(w-1)], y <- [0..(h-1)] ]

build :: Int -> Int -> Display
build w h = array ( (0, 0), (w-1, h-1) ) (box w h False)

execute :: Display -> Cmd -> Display
execute d (Rect w h) = d//(box w h True)
execute d (Row y n) =
    let ((_, _), (w, h)) = bounds d
        ps = filter (\((_, y'), _) -> y == y') (assocs d)
        ps' = map (\((x, y), v) -> (((x+n) `mod` (w+1), y), v)) ps
    in d//ps'
execute d (Col x n) =
    let ((_, _), (w, h)) = bounds d
        ps = filter (\((x', _), _) -> x == x') (assocs d)
        ps' = map (\((x, y), v) -> ((x, (y+n) `mod` (h+1)), v)) ps
    in d//ps'

gasi :: (Read a, Num a) => String -> String -> [a]
gasi str regex = map read (regexgrps str regex)

{-
    "rect 1x2"
    "rotate row y=0 by 3"
    "rotate column x=4 by 5"
-}
parse :: String -> Cmd
parse s
    | "rect"  `L.isInfixOf` s =
        let [w, h] = gasi s "rect ([0-9]+)x([0-9]+)"
        in Rect w h
    | "row"   `L.isInfixOf` s =
        let [y, n] = gasi s "rotate row y=([0-9]+) by ([0-9]+)"
        in Row y n
    | "colum" `L.isInfixOf` s =
        let [x, n] = gasi s "rotate column x=([0-9]+) by ([0-9]+)"
        in Col x n

count_lit :: Display -> Int
count_lit d = length $ filter snd (assocs d)

draw :: Display -> [String]
draw d =
    let ((_, _), (w, h)) = bounds d
        ls = concat $ take (h+1) $ repeat (take (w+1) $ repeat '.')
        ls' = foldl (\ls ((x, y), _) -> update ((w+1) * y + x) '#' ls) ls (filter snd (assocs d))
    in map T.unpack (T.chunksOf (w+1) (T.pack ls'))

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
    let d = foldl execute (build 7 3) [(Rect 1 3), (Rect 3 2), (Rect 6 1)]
    write d
    print $ assert (["######.",
                     "###....",
                     "#......"] == (draw d)) "ad hoc passed!"
    print $ assert (10 == (count_lit d)) "count ad hoc"
    let d = execute (build 7 3) (Rect 3 2)
    write d
    print $ assert (["###....",
                     "###....",
                     "......."] == (draw d)) "step one passed!"
    print $ assert (6 == (count_lit d)) "count one"
    let d = foldl execute (build 7 3) [(Rect 3 2), (Col 1 1)]
    write d
    print $ assert (["#.#....",
                     "###....",
                     ".#....."] == (draw d)) "step two passed!"
    print $ assert (6 == (count_lit d)) "count two"
    let d = foldl execute (build 7 3) [(Rect 3 2), (Col 1 1), (Row 0 4)]
    write d
    print $ assert (["....#.#",
                     "###....",
                     ".#....."] == (draw d)) "step three passed!"
    print $ assert (6 == (count_lit d)) "count three"
    let d = foldl execute (build 7 3) [(Rect 3 2), (Col 1 1), (Row 0 4), (Col 1 1)]
    write d
    print $ assert ([".#..#.#",
                     "#.#....",
                     ".#....."] == (draw d)) "step four passed!"
    print $ assert (6 == (count_lit d)) "count four"

    let cs = map parse [
            "rect 1x2",
            "rotate row y=0 by 3",
            "rotate column x=4 by 5"
            ]
    print cs
    print $ assert ([
                    Rect 1 2,
                    Row 0 3,
                    Col 4 5
                    ] == cs) "parsing passed"

    let cmds = (map parse (lines input))

    let r = part_one cmds
    print r
    print $ assert (110 == r) "part one passed!"
    write (foldl execute (build 50 6) cmds)
--    let r = part_two input
--    print r
--    print $ assert (0 == r) "part two passed!"
