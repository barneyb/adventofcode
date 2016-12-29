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

-- my 'containers' package is too old to have this
deleteAt :: Int -> S.Seq a -> S.Seq a
deleteAt i s = S.take i s S.>< S.drop (i+1) s

part_two :: Int -> Int
part_two n = go (S.fromList [1..n])
     where
         go :: S.Seq Int -> Int
         go s
             | len == 1  = tip
             | otherwise =
                let s' = deleteAt (len `div` 2) s
                in go ((S.drop 1 s') S.|> tip)
             where
                len = S.length s
                tip = s `S.index` 0

main = do
    assert_equal 3 (part_one 5) "example one"
    assert_equal 5 (part_one 10) "test one"
    assert_equal 3 (part_one 9) "test two"
    assert_equal 15 (part_one 23) "test three"

    assert_equal 1816277 (part_one 3005290) "part one"

    assert_equal 2 (part_two 5) "example two"

    assert_equal 1410967 (part_two 3005290) "part two"
