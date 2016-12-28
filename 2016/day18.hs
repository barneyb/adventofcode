import Control.Exception (assert)
import Utils

is_trap :: (Bool, Bool, Bool) -> Bool
is_trap (True, True, False) = True
is_trap (False, True, True) = True
is_trap (True, False, False) = True
is_trap (False, False, True) = True
is_trap _ = False

next_line :: String -> String
next_line s =
    let ts = False : (map (== '^') s) ++ [False]
        ts' = map (\i -> is_trap (ts !! (i-1), ts !! i, ts !! (i+1))) [1..(length s)]
    in map (\v -> if v then '^' else '.') ts'

tiles :: String -> Int -> [String]
tiles first_row row_count = scanl (\l _ -> next_line l) first_row [2..row_count]

part_one :: String -> Int -> Int
part_one first_row row_count = length $ filter (== '.') $ concat $ tiles first_row row_count

--part_two :: String -> Int
--part_two input = length input

input_one = "..^^."
input_two = ".^^.^.^^^^"

main = do
    let input = ".^^^^^.^^^..^^^^^...^.^..^^^.^^....^.^...^^^...^^^^..^...^...^^.^.^.......^..^^...^.^.^^..^^^^^...^."

    assert_equal ".^^^^" (next_line "..^^.") "next line 1.1"
    assert_equal "^^..^" (next_line ".^^^^") "next line 1.2"

    assert_equal "^^^...^..^" (next_line ".^^.^.^^^^") "next line 2.1"
    assert_equal "^.^^.^.^^." (next_line "^^^...^..^") "next line 2.2"
    assert_equal "..^^...^^^" (next_line "^.^^.^.^^.") "next line 2.3"
    assert_equal ".^^^^.^^.^" (next_line "..^^...^^^") "next line 2.4"
    assert_equal "^^..^.^^.." (next_line ".^^^^.^^.^") "next line 2.5"
    assert_equal "^^^^..^^^." (next_line "^^..^.^^..") "next line 2.6"
    assert_equal "^..^^^^.^^" (next_line "^^^^..^^^.") "next line 2.7"
    assert_equal ".^^^..^.^^" (next_line "^..^^^^.^^") "next line 2.8"
    assert_equal "^^.^^^..^^" (next_line ".^^^..^.^^") "next line 2.9"

    assert_equal [ "..^^."
                 , ".^^^^"
                 , "^^..^"
                 ] (tiles "..^^." 3) "board one"

    assert_equal [ ".^^.^.^^^^"
                 , "^^^...^..^"
                 , "^.^^.^.^^."
                 , "..^^...^^^"
                 , ".^^^^.^^.^"
                 , "^^..^.^^.."
                 , "^^^^..^^^."
                 , "^..^^^^.^^"
                 , ".^^^..^.^^"
                 , "^^.^^^..^^"
                 ] (tiles ".^^.^.^^^^" 10) "board two"

    assert_equal 6 (part_one input_one 3) "example one"
    assert_equal 38 (part_one input_two 10) "example two"

    assert_equal 1956 (part_one input 40) "part one"

--     assert_equal 0 (part_two input) "part two"
