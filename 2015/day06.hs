import Control.Exception (assert)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import qualified Data.List.Split as S
import qualified Data.List as L
import Data.Maybe

type Light = (Int, Int)

data Op = On | Off | Toggle

type Instruction = (Op, Light, Light)

type Executable = (Op, [Light])

parse_light :: String -> Light
parse_light coords =
    let [x, y] = map read (S.splitOn "," coords)
    in (x, y)

parse_line :: String -> Instruction
parse_line line
    | L.isInfixOf "turn off" line = (Off, (parse_light (parts !! 2)), (parse_light (parts !! 4)))
    | L.isInfixOf "turn on" line = (On, (parse_light (parts !! 2)), (parse_light (parts !! 4)))
    | L.isInfixOf "toggle" line = (Toggle, (parse_light (parts !! 1)), (parse_light (parts !! 3)))
    where parts = words line

to_exec :: Instruction -> Executable
to_exec (op, s, e) = (op, [ (x, y) | x <- [(fst s)..(fst e)], y <- [(snd s)..(snd e)] ])

execs :: String -> [Executable]
execs input = map to_exec (map parse_line (lines input))

minus :: Ord a => Set.Set a -> Set.Set a -> Set.Set a
minus = Set.foldl' (flip Set.delete)

count_lit :: String -> Int
count_lit input = Set.size $ foldl execute Set.empty (execs input)
    where
        execute :: Set.Set Light -> Executable -> Set.Set Light
        execute grid (On, lights) = Set.union grid (Set.fromList lights)
        execute grid (Off, lights) = minus grid (Set.intersection grid (Set.fromList lights))
        execute grid (Toggle, lights) =
            let
                ls = (Set.fromList lights)
                offs = Set.intersection grid ls
            in minus (Set.union grid ls) offs

sum_brightness :: String -> Int
sum_brightness input = Map.foldl' (+) 0 (foldl execute Map.empty (execs input))
    where
        add :: Int -> Maybe Int -> Maybe Int
        add x Nothing = Just x
        add x (Just y) = Just (x + y)
        sub :: Int -> Maybe Int -> Maybe Int
        sub x Nothing = Nothing
        sub x (Just y)
            | x >= y    = Nothing
            | otherwise = Just (y - x)
        execute :: Map.Map Light Int -> Executable -> Map.Map Light Int
        execute grid (On, lights) = L.foldl' (\g l -> Map.alter (add 1) l g) grid lights
        execute grid (Off, lights) = L.foldl' (\g l -> Map.alter (sub 1) l g) grid lights
        execute grid (Toggle, lights) = L.foldl' (\g l -> Map.alter (add 2) l g) grid lights

main = do
    input <- readFile "day06_input.txt"
    print $ assert (0 == count_lit "turn off 0,0 through 1,1") "zero worked"
    print $ assert (25 == count_lit "turn on 1,1 through 5,5") "on worked"
    print $ assert (25 - 4 == count_lit (
            "turn on 1,1 through 5,5\n" ++
            "turn off 2,2 through 3,3\n"
        )) "off worked"
    print $ assert (25 - 5 == count_lit (
            "turn on 1,1 through 5,5\n" ++
            "toggle 1,1 through 5,1\n"
        )) "toggle worked"
    print $ assert (10000 - 200 - 196 + 4 == count_lit (
            "turn on 0,0 through 99,99\n" ++ -- 10,000 on
            "turn off 0,50 through 99,51\n" ++ -- 200 off
            "toggle 50,0 through 51,99\n" -- 196 off, 4 on
        )) "bigger stuff worked"

    print $ assert (0 == sum_brightness "turn off 0,0 through 0,0") "zero worked"
    print $ assert (50 == sum_brightness (
            "turn on 1,1 through 5,5\n" ++
            "turn on 1,1 through 5,5"
        )) "on worked"
    print $ assert (25 == sum_brightness (
            "turn on 1,1 through 5,5\n" ++
            "turn on 1,1 through 5,5\n" ++
            "turn off 1,1 through 5,5"
        )) "off worked"
    print $ assert (0 == sum_brightness (
            "turn on 1,1 through 5,5\n" ++
            "turn off 1,1 through 5,5\n" ++
            "turn off 1,1 through 5,5\n" ++
            "turn off 1,1 through 5,5"
        )) "off worked"
    print $ assert (100 == sum_brightness (
            "toggle 1,1 through 5,5\n" ++
            "toggle 1,1 through 5,5"
        )) "toggle worked"

--    print $ assert (569999 == count_lit input) "part one passed"
--    print $ assert (17836115 == sum_brightness input) "part two passed"
