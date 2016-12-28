import Control.Exception (assert)
import qualified Data.Sequence as S
import Debug.Trace
import Utils

part_one :: Int -> Int
part_one n = go (S.fromList [1..n])
    where
        go :: S.Seq Int -> Int
        go s
            | (S.length s) == 1 = tip
            | otherwise         = go ((S.drop 2 s) S.|> tip)
            where tip = s `S.index` 0

--part_two :: String -> Int
--part_two input = length input

main = do
    assert_equal 3 (part_one 5) "example one"
    assert_equal 5 (part_one 10) "test one"
    assert_equal 3 (part_one 9) "test two"
    assert_equal 15 (part_one 23) "test three"

    assert_equal 1816277 (part_one 3005290) "part one"

--     assert_equal 0 (part_two 3005290) "part two"
