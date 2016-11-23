import Control.Exception (assert)
import qualified Data.Set as Set

type Pos = (Int, Int)
type Dir = (Int, Int)

move :: Pos -> Dir -> Pos
move (px, py) (dx, dy) = (px + dx, py + dy)

to_dir :: Char -> Dir
to_dir '<' = (-1,  0)
to_dir '^' = ( 0,  1)
to_dir '>' = ( 1,  0)
to_dir 'v' = ( 0, -1)

house_set :: String -> Set.Set Pos
house_set input = Set.fromList $ scanl move (0, 0) (map to_dir input)

house_count :: String -> Int
house_count = Set.size . house_set

every_second :: [a] -> [a]
every_second xs = map fst $ filter (odd . snd) (zip xs [1..])
--every_second (x:y:xs) = x : every_second xs
--every_second (x:[]) = []
--every_second [] = []

robo_house_count :: String -> Int
robo_house_count input = Set.size $ Set.union (house_set (every_second input)) (house_set (every_second (tail input)))

main = do
    input <- readFile "day03_input.txt"
    print $ assert (2592 == (house_count input)) "part one passed"
    print $ assert (2360 == (robo_house_count input)) "part two passed"
