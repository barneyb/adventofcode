import qualified Data.List as L
import Debug.Trace
import Utils

data Cmd = Move Int Int         -- move position 1 to position 4
         | Reverse Int Int      -- reverse positions 0 through 4
         | RotateRight Int      -- rotate right 1 step
         | RotateLeft Int       -- rotate left 2 step
         | RotateIndex Char     -- rotate based on position of letter b
         | SwapLetter Char Char -- swap letter d with letter b
         | SwapPosition Int Int -- swap position 4 with position 0
         deriving (Eq, Show)

execute :: String -> Cmd -> String
execute s (SwapPosition x y) =
    let a = s!!x
        b = s!!y
    in (take (x) s) ++ [b] ++ (take (y - x - 1) (drop (x + 1) s)) ++ [a] ++ (drop (y + 1) s)
execute s (SwapLetter a b) =
    let Just x = (L.elemIndex a s)
        Just y = (L.elemIndex b s)
    in execute s (SwapPosition (min x y) (max x y))
execute s (RotateLeft n) =
    let n' = n `mod` (length s)
        pre = take n' s
    in (drop n' s) ++ pre
execute s (RotateRight n) =
    let n' = (length s) - n
    in execute s (RotateLeft n')
execute s (RotateIndex c) =
    let Just i = (L.elemIndex c s)
        n = i + (if i >= 4 then 2 else 1)
    in execute s (RotateRight n)
execute s (Reverse x y) =
    let pre = take (x) s
        mid = take (y - x + 1) (drop (x) s)
        suf = drop (y + 1) s
    in pre ++ (reverse mid) ++ suf
execute s (Move x y) =
    let s' = (take x s) ++ (drop (x + 1) s)
    in (take y s') ++ [s!!x] ++ (drop y s')

run :: [Cmd] -> String -> [String]
run is s = L.scanl execute s is

parse :: String -> Cmd
parse s = Move 0 0

part_one :: String -> String -> String
part_one input passwd =
    let cmds = map parse (lines input)
    in last (run cmds passwd)

--part_two :: String -> Int
--part_two input = length input

test_input = "swap position 4 with position 0\n\
             \swap letter d with letter b\n\
             \reverse positions 0 through 4\n\
             \rotate right 1 step\n\
             \rotate left 2 step\n\
             \move position 1 to position 4\n\
             \move position 3 to position 0\n\
             \rotate based on position of letter b\n\
             \rotate based on position of letter d"

test_cmds = [ SwapPosition 0 4 -- parse sorts these!
            , SwapLetter 'd' 'b'
            , Reverse 0 4 -- parse sorts these!
            , RotateRight 1
            , RotateLeft 2
            , Move 1 4
            , Move 3 0
            , RotateIndex 'b'
            , RotateIndex 'd'
            ]

test_scan = [ "abcde"
            , "ebcda"
            , "edcba"
            , "abcde"
            , "eabcd"
            , "bcdea"
            , "bdeac"
            , "abdec"
            , "ecabd"
            , "decab"
            ]

main = do
    input <- readFile "day21_input.txt"

    assert_equal "acdbe" (execute "abcde" (Move 1 3)) "move"
    assert_equal "bcdae" (execute "abcde" (Move 0 3)) "move 2"
    assert_equal "dabce" (execute "abcde" (Move 3 0)) "move 3"

    assert_equal "adcbe" (execute "abcde" (Reverse 1 3)) "reverse"

    assert_equal "adcbe" (execute "abcde" (SwapPosition 1 3)) "swap position"
    assert_equal "edcba" (execute "abcde" (SwapPosition 0 4)) "swap position 2"

    assert_equal test_cmds (map parse (lines test_input)) "test parse"

    assert_equal test_scan (run test_cmds "abcde") "test run"

    assert_equal "decab" (part_one test_input "abcde") "example one"

--     assert_equal "-------" (part_one input "abcdefgh") "part one"

--     assert_equal 0 (part_two input) "part two"
