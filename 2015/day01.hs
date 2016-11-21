delta :: Char -> Int
delta '(' = 1
delta ')' = -1

find_floor :: [Char] -> Int
find_floor cs = sum (map delta cs)

bfold :: (Int, Int, Int) -> Int -> (Int, Int, Int)
bfold (p, c, f) d
    | p >= 0    = (p, 0, 0)
    | n < 0     = (c + 1, 0, 0)
    | otherwise = (-1, c + 1, n)
    where n = f + d

bpos :: [Char] -> Int
bpos cs =
    let (r, _, _) = foldl bfold (-1, 0, 0) (map delta cs)
    in r

main = do
    input <- readFile "day01_input.txt"
    print (find_floor input)
    print (bpos input)
