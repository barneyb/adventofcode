import qualified Data.List as L
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
execute s (SwapPosition x y) = s
execute s (SwapLetter a b) = s
execute s (RotateLeft n) = s
execute s (RotateRight n) = s
execute s (RotateIndex c) = s
execute s (Reverse x y) = s
execute s (Move x y) = s

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

test_cmds = [ SwapPosition 4 0
            , SwapLetter 'd' 'b'
            , Reverse 0 4
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

    assert_equal test_cmds (map parse (lines test_input)) "test parse"

    assert_equal test_scan (run test_cmds "abcde") "test run"

    assert_equal "decab" (part_one test_input "abcde") "example one"

--     assert_equal "-------" (part_one input "abcdefgh") "part one"

--     assert_equal 0 (part_two input) "part two"
