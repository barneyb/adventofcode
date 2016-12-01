import Control.Exception (assert)

data Turn = L | R deriving (Eq, Show)

data Heading = N | E | S | W deriving (Eq, Enum, Show)

type Position = (Heading, Int, Int)

type Step = (Turn, Int)

type Delta = (Int, Int)

steps :: String -> [Step]
steps input = map parse (words input)
    where
        parse :: String -> Step
        parse s = p c
            where
                p ('L':ds) = (L, read ds)
                p ('R':ds) = (R, read ds)
                c = if (last s) == ',' then (init s) else s

turn :: Heading -> Turn -> Heading
turn N L = W
turn h L = pred h
turn W R = N
turn h R = succ h

delta :: Heading -> Int -> Delta
delta N n = (0, n)
delta E n = (n, 0)
delta S n = (0, -n)
delta W n = (-n, 0)

walk :: Position -> Step -> Position
walk (h, x, y) (t, n) =
    let
        h_ = turn h t
        (dx, dy) = delta h_ n
    in (h_, x + dx, y + dy)

part_one :: String -> Int
part_one input =
    let
        (h, x, y) = foldl walk (N, 0, 0) (steps input)
    in (abs x) + (abs y)

main = do
    input <- readFile "day01_input.txt"
    print $ assert (5 == (part_one "R2, L3")) "test one passed!"
    print $ assert (2 == (part_one "R2, R2, R2")) "test two passed!"
    print $ assert (12 == (part_one "R5, L5, R5, R3")) "test three passed!"
    print $ assert (288 == (part_one input)) "part one passed!"
