import Control.Exception (assert)

offset :: Int -> Char -> Int
offset c 'L'
    | c `notElem` [1, 4, 7] = c - 1
offset c 'R'
    | c `notElem` [3, 6, 9] = c + 1
offset c 'U'
    | c `notElem` [1, 2, 3] = c - 3
offset c 'D'
    | c `notElem` [7, 8, 9] = c + 3
offset c _ = c

start :: Int
start = 5

do_line :: Int -> String -> Int
do_line c line = foldl offset c line

part_one :: String -> String
part_one input = foldl1 (++) (map show (tail $ scanl do_line start (lines input)))

main = do
    input <- readFile "day02_input.txt"
    print $ assert ("1985" == (part_one "ULL\nRRDDD\nLURDL\nUUUUD")) "test one passed!"
    print $ assert ("61529" == (part_one input)) "part one passed!"
