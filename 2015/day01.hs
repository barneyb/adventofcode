import Control.Exception (assert)

delta :: Char -> Int
delta '(' = 1
delta ')' = -1

find_floor :: String -> Int
find_floor s = sum (map delta s)

type PosCountFloor = (Int, Int, Int)

bfold :: PosCountFloor -> Int -> PosCountFloor
bfold (p, c, f) d
    | p >= 0    = (p, 0, 0)
    | n < 0     = (c + 1, 0, 0)
    | otherwise = (-1, c + 1, n)
    where n = f + d

bpos :: String -> Int
bpos s =
    let (r, _, _) = foldl bfold (-1, 0, 0) (map delta s)
    in r

main = do
    input <- readFile "day01_input.txt"
    print $ assert (232 == (find_floor input)) "part one passed!"
    print $ assert (1783 == (bpos input)) "part two passed!"
