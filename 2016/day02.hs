import Control.Exception (assert)

data Move = L | R | D | U deriving (Eq, Show)

type Button = Char

parse_move :: Char -> Move
parse_move 'R' = R
parse_move 'L' = L
parse_move 'D' = D
parse_move 'U' = U

offset :: Button -> Move -> Button
offset c L
    | c `notElem` ['1', '4', '7'] = pred c
offset c R
    | c `notElem` ['3', '6', '9'] = succ c
offset c U
    | c `notElem` ['1', '2', '3'] = pred $ pred $ pred c
offset c D
    | c `notElem` ['7', '8', '9'] = succ $ succ $ succ c
offset c _ = c

start :: Button
start = '5'

do_line :: Button -> String -> Button
do_line c line = foldl offset c (map parse_move line)

part_one :: String -> String
part_one input = tail $ scanl do_line start (lines input)

test_input :: String
test_input = "ULL\nRRDDD\nLURDL\nUUUUD"

main = do
    input <- readFile "day02_input.txt"
    print $ assert ("1985" == (part_one test_input)) "test one passed!"
    print $ assert ("61529" == (part_one input)) "part one passed!"
