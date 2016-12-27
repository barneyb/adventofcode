import Control.Exception (assert)
import qualified Data.List as L
import Utils
import Data.Function.Memoize

find_triple :: String -> Maybe Char
find_triple (x:y:[]) = Nothing
find_triple (x:y:z:xs)
    | x == y && x == z = Just x
    | otherwise        = find_triple (y:z:xs)

has_five :: (Int -> String) -> Int -> Char -> Bool
has_five hash n c =
    let five = [c, c, c, c, c]
    in any (five `L.isInfixOf`) (map hash [(n+1)..(n+1000)])

is_key :: (Int -> String) -> (Int, String) -> Bool
is_key hash (n, h) =
    let mc = find_triple h
    in case mc of Just c  -> has_five hash n c
                  Nothing -> False

hashes :: (Int -> String) -> [(Int, String)]
hashes hash = filter (is_key hash) (map (\i -> (i, hash i)) [0..])

search :: Int -> (Int -> String) -> Int
search n h =
    let hs = drop (n - 1) (hashes h)
    in fst $ head hs

part_one :: String -> Int
part_one salt = search 64 (nhash salt)

part_two :: String -> Int
part_two salt = search 64 (memoize (snhash 2016 salt))

input = "ngcjuoqr"
test_input = "abc"

main = do

    print $ assert ("577571be4de9dcce85a041ba0410f29f" == (nhash "abc" 0)) "hash test one passed"

    let r = part_one test_input
    print r
    print $ assert (22728 == r) "example one passed!"

--     let r = part_one input
--     print r
--     print $ assert (18626 == r) "part one passed!"

    print $ assert ("577571be4de9dcce85a041ba0410f29f" == (snhash 0 "abc" 0)) "stretch test zero passed"
    print $ assert ("eec80a0c92dc8a0777c619d9bb51e910" == (snhash 1 "abc" 0)) "stretch test one passed"
    print $ assert ("16062ce768787384c81fe17a7a60c7e3" == (snhash 2 "abc" 0)) "stretch test two passed"
    print $ assert ("a107ff634856bb300138cac6568c0f24" == (snhash 2016 "abc" 0)) "stretch test 2016 passed"

    print $ assert (10 == (search 1 (traceMemoize (snhash 2016 "abc")))) "example test one passed"

    -- this takes a couple minutes to run....
--     let r = part_two test_input
--     print r
--     print $ assert (22551 == r) "example two passed!"

    -- this takes a couple minutes to run....
    let r = part_two input
    print r
    print $ assert (20092 == r) "part two passed!"
