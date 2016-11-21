delta :: Char -> Int
delta '(' = 1
delta ')' = -1

find_floor :: [Char] -> Int
find_floor cs = sum (map delta cs)

main = do
    input <- readFile "day01_input.txt"
    print (find_floor input)
