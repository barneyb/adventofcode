import Control.Exception (assert)

data Turn = L | R deriving (Eq, Show)

data Heading = N | E | S | W deriving (Eq, Enum, Show)

data Position = Pos Heading Int Int

instance Eq Position where
    (Pos _ x1 y1) == (Pos _ x2 y2) = x1 == x2 && y1 == y2

instance Show Position where
    show (Pos h x y) = "(" ++ show h ++ ", " ++ show x ++ ", " ++ show y ++ ")"

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

start :: Position
start = Pos N 0 0

dist :: Position -> Position -> Int
dist (Pos _ x1 y1) (Pos _ x2 y2) = (abs x2 - x1) + (abs y2 - y1)

distFromStart :: Position -> Int
distFromStart = dist start

walk :: Position -> Step -> Position
walk (Pos h x y) (t, n) =
    let
        h_ = turn h t
        (dx, dy) = delta h_ n
    in Pos h_ (x + dx) (y + dy)

part_one :: String -> Int
part_one input = distFromStart $ foldl walk start (steps input)

expand_steps :: (Position, Position) -> [Position]
expand_steps (Pos _ x1 y1, Pos h x2 y2) = case h of
    N -> [ Pos h x1 y | y <- [(y1+1)..y2] ]
    E -> [ Pos h x y1 | x <- [(x1+1)..x2] ]
    S -> [ Pos h x1 y | y <- [(y1-1),(y1-2)..y2] ]
    W -> [ Pos h x y1 | x <- [(x1-1),(x1-2)..x2] ]

part_two :: String -> Int
part_two input =
    let
        ps = scanl walk start (steps input)
        pps = zip ps (tail ps)
        exps = map expand_steps pps
        locs = foldl (\a v -> a ++ v) [start] exps
        findDupes :: [Position] -> [(Bool, Position)]
        findDupes [] = []
        findDupes (p:ps) = (p `elem` ps, p):(findDupes ps)
        ds = dropWhile (\x -> not (fst x)) (findDupes locs)
        (_, d) = head ds
    in distFromStart d

main = do
    input <- readFile "day01_input.txt"
    print $ assert (5 == (part_one "R2, L3")) "test one passed!"
    print $ assert (2 == (part_one "R2, R2, R2")) "test two passed!"
    print $ assert (12 == (part_one "R5, L5, R5, R3")) "test three passed!"
    print $ assert (288 == (part_one input)) "part one passed!"
    print $ assert (4 == (part_two "R8, R4, R4, R8, L200")) "test four passed!"
    print $ assert (111 == (part_two input)) "part two passed!"
