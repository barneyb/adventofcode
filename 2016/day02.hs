import Control.Exception (assert)

data Move = L | R | D | U deriving (Eq, Show)

type Button = Char

parse_move :: Char -> Move
parse_move 'R' = R
parse_move 'L' = L
parse_move 'D' = D
parse_move 'U' = U

{-
1 2 3
4 5 6
7 8 9
-}
move1 :: Button -> Move -> Button
move1 c L
    | c `notElem` ['1', '4', '7'] = pred c
move1 c R
    | c `notElem` ['3', '6', '9'] = succ c
move1 c U
    | c `notElem` ['1', '2', '3'] = pred $ pred $ pred c
move1 c D
    | c `notElem` ['7', '8', '9'] = succ $ succ $ succ c
move1 c _ = c

{-
    1
  2 3 4
5 6 7 8 9
  A B C
    D
-}
move2 :: Button -> Move -> Button
move2 c d = (moves c) !! (idx d)
    where
        idx :: Move -> Int
        idx L = 0
        idx R = 1
        idx D = 2
        idx U = 3
        moves :: Button -> [Button]
                  --  L    R    D    U
        moves '1' = ['1', '1', '3', '1']
        moves '2' = ['2', '3', '6', '2']
        moves '3' = ['2', '4', '7', '1']
        moves '4' = ['3', '4', '8', '4']
        moves '5' = ['5', '6', '5', '5']
        moves '6' = ['5', '7', 'A', '2']
        moves '7' = ['6', '8', 'B', '3']
        moves '8' = ['7', '9', 'C', '4']
        moves '9' = ['8', '9', '9', '9']
        moves 'A' = ['A', 'B', 'A', '6']
        moves 'B' = ['A', 'C', 'D', '7']
        moves 'C' = ['B', 'C', 'C', '8']
        moves 'D' = ['D', 'D', 'D', 'B']

start :: Button
start = '5'

do_line :: (Button -> Move -> Button) -> Button -> String -> Button
do_line m c line = foldl m c (map parse_move line)

do_code :: (Button -> Move -> Button) -> String -> String
do_code m s = tail $ scanl (do_line m) start (lines s)

part_one :: String -> String
part_one = do_code move1

test_input :: String
test_input = "ULL\nRRDDD\nLURDL\nUUUUD"

part_two :: String -> String
part_two = do_code move2

main = do
    input <- readFile "day02_input.txt"
    print $ assert ("1985" == (part_one test_input)) "test one passed!"
    print $ assert ("61529" == (part_one input)) "part one passed!"
    print $ assert ("5DB3" == (part_two test_input)) "test two passed!"
    print $ assert ("C2C28" == (part_two input)) "test two passed!"
