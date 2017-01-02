import qualified Data.List as L
import qualified Data.Sequence as S
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
execute s (SwapPosition x' y') =
    let x = min x' y'
        y = max x' y'
        a = s!!x
        b = s!!y
    in (take (x) s) ++ [b] ++ (take (y - x - 1) (drop (x + 1) s)) ++ [a] ++ (drop (y + 1) s)
execute s (SwapLetter a b) =
    let Just x = (L.elemIndex a s)
        Just y = (L.elemIndex b s)
    in execute s (SwapPosition x y)
execute s (RotateLeft n) = rotate_left s n
execute s (RotateRight n) =
    let n' = (length s) - n
    in execute s (RotateLeft n')
execute s (RotateIndex c) =
    let Just i = (L.elemIndex c s)
        n = i + (if i >= 4 then 2 else 1)
    in execute s (RotateRight n)
execute s (Reverse x' y') =
    let x = min x' y'
        y = max x' y'
        pre = take (x) s
        mid = take (y - x + 1) (drop (x) s)
        suf = drop (y + 1) s
    in pre ++ (reverse mid) ++ suf
execute s (Move x y) =
    let s' = (take x s) ++ (drop (x + 1) s)
    in (take y s') ++ [s!!x] ++ (drop y s')

rotate_left :: String -> Int -> String
rotate_left s n =
    let n' = n `mod` (length s)
        pre = take n' s
    in (drop n' s) ++ pre

unexecute :: String -> Cmd -> String
unexecute s (SwapPosition x y) = execute s (SwapPosition y x)
unexecute s (SwapLetter a b) = execute s (SwapLetter b a)
unexecute s (RotateLeft n) = execute s (RotateRight n)
unexecute s (RotateRight n) = execute s (RotateLeft n)
unexecute s (RotateIndex c) =
    let s's = map (\i -> rotate_left s i) [0..((length s) - 1)]
        s' = filter (\s' -> s == execute s' (RotateIndex c)) s's
    in head s'
unexecute s (Move x y) = execute s (Move y x)
unexecute s (Reverse x y) = execute s (Reverse x y)

run :: [Cmd] -> String -> [String]
run is s = L.scanl execute s is

unrun :: [Cmd] -> String -> [String]
unrun is s = L.scanl unexecute s (reverse is)

parse :: String -> Cmd
parse s
    | L.isPrefixOf "swap position" s =
        let ns = map read (regexgrps s "swap position ([0-9]+) with position ([0-9]+)")
        in (SwapPosition (ns!!0) (ns!!1))
    | L.isPrefixOf "swap letter" s =
        let cs = regexgrps s "swap letter ([a-z]) with letter ([a-z])"
        in (SwapLetter (cs!!0!!0) (cs!!1!!0))
    | L.isPrefixOf "reverse positions" s =
        let ns = map read (regexgrps s "reverse positions ([0-9]+) through ([0-9]+)")
        in (Reverse (ns!!0) (ns!!1))
    | L.isPrefixOf "rotate right" s =
        let ns = map read (regexgrps s "rotate right ([0-9]+) step")
        in (RotateRight (ns!!0))
    | L.isPrefixOf "rotate left" s =
        let ns = map read (regexgrps s "rotate left ([0-9]+) step")
        in (RotateLeft (ns!!0))
    | L.isPrefixOf "rotate based" s =
        let ns = regexgrps s "rotate based on position of letter ([a-z])"
        in (RotateIndex (ns!!0!!0))
    | L.isPrefixOf "move" s =
        let ns = map read (regexgrps s "move position ([0-9]+) to position ([0-9]+)")
        in (Move (ns!!0) (ns!!1))

part_one :: String -> String -> String
part_one input passwd =
    let cmds = map parse (lines input)
    in last (run cmds passwd)

part_two :: String -> String -> String
part_two input passwd =
    let cmds = map parse (lines input)
    in last (unrun cmds passwd)

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

    assert_equal "acdbe" (execute "abcde" (Move 1 3)) "move"
    assert_equal "bcdae" (execute "abcde" (Move 0 3)) "move 2"
    assert_equal "dabce" (execute "abcde" (Move 3 0)) "move 3"

    assert_equal "adcbe" (execute "abcde" (Reverse 1 3)) "reverse"

    assert_equal "adcbe" (execute "abcde" (SwapPosition 1 3)) "swap position"
    assert_equal "ebcda" (execute "abcde" (SwapPosition 0 4)) "swap position 2"

    assert_equal test_cmds (map parse (lines test_input)) "test parse"

    assert_equal test_scan (run test_cmds "abcde") "test run"

    foldl1 (>>) (map (\c ->
        assert_equal "abcde" (unexecute (execute "abcde" c) c) ("test symmetry on " ++ (show c))) test_cmds)

    assert_equal "decab" (part_one test_input "abcde") "example one"

    assert_equal "gfdhebac" (part_one input "abcdefgh") "part one"

    assert_equal "eabcd" (execute "abcde" (RotateIndex 'a')) "rotate a"
    assert_equal "deabc" (execute "abcde" (RotateIndex 'b')) "rotate b"
    assert_equal "cdeab" (execute "abcde" (RotateIndex 'c')) "rotate c"
    assert_equal "bcdea" (execute "abcde" (RotateIndex 'd')) "rotate d"
    assert_equal "eabcd" (execute "abcde" (RotateIndex 'e')) "rotate e"

    assert_equal "cdeab" (execute "deabc" (RotateIndex 'c')) "rotate c 1"
    assert_equal "cdeab" (execute "abcde" (RotateIndex 'c')) "rotate c 1"

    assert_equal "abcde" (unexecute "eabcd" (RotateIndex 'a')) "unrotate a"
    assert_equal "abcde" (unexecute "deabc" (RotateIndex 'b')) "unrotate b"
    -- this one ends up with two equvalent rotations, which is "illegal" (because it's not unambiguously reversable)
--     assert_equal "abcde" (unexecute "cdeab" (RotateIndex 'c')) "unrotate c"
    assert_equal "abcde" (unexecute "bcdea" (RotateIndex 'd')) "unrotate d"
    assert_equal "abcde" (unexecute "eabcd" (RotateIndex 'e')) "unrotate e"

    assert_equal (reverse test_scan) (unrun test_cmds "decab") "test unrun"

    assert_equal "abcde" (part_two test_input "decab") "example two"

    assert_equal "abcdefgh" (part_two input "gfdhebac") "part one two"

    assert_equal "dhaegfbc" (part_two input "fbgdceah") "part two"

