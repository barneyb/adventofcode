import Control.Exception (assert)
import qualified Data.List as L
import Utils

find_triple :: String -> Maybe Char
find_triple (x:y:[]) = Nothing
find_triple (x:y:z:xs)
    | x == y && x == z = Just x
    | otherwise        = find_triple (y:z:xs)

has_five :: String -> Int -> Char -> Bool
has_five salt n c =
    let five = [c, c, c, c, c]
    in any (five `L.isInfixOf`) (map (nhash salt) [(n+1)..(n+1000)])

is_key :: String -> (Int, String) -> Bool
is_key salt (n, h) =
    let mc = find_triple h
    in case mc of Just c  -> has_five salt n c
                  Nothing -> False

hashes :: String -> [(Int, String)]
hashes salt = filter (is_key salt) (map (\i -> (i, nhash salt i)) [0..])

part_one :: String -> Int
part_one salt =
    let hs = drop 63 (hashes salt)
    in fst $ head hs

--part_two :: String -> Int
--part_two input = length input

input = "ngcjuoqr"
test_input = "abc"

main = do

    let r = part_one test_input
    print r
    print $ assert (22728 == r) "example one passed!"

    let r = part_one input
    print r
    print $ assert (18626 == r) "part one passed!"

--     let r = part_two input
--     print r
--     print $ assert (0 == r) "part two passed!"
