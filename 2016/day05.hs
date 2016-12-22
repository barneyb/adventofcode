import Control.Exception (assert)
import Utils

candidates :: String -> [String]
candidates key = filter ((== "00000") . (take 5)) (map (nhash key) [0..])

pass :: Int -> String -> String
pass len key = take len (map (!! 5) (candidates key))

part_one :: String -> String
part_one = pass 8

part_two :: String -> String
part_two key =
    let dpairs = map (\c -> (c !! 5, c !! 6)) (candidates key)
        cpairs = filter (\ c -> ((fst c) `elem` ['0'..'7'])) dpairs
        pairs = map (\ (p, c) -> (read (p:[]), c)) cpairs
        m :: String -> (Int, Char) -> String
        m str (i, c)
            | str !! i /= '_' = str
            | otherwise       = update i c str
    in head $ dropWhile ('_' `elem`) (scanl m "________" pairs)

test_input = "abc"
input = "reyedfim"

main = do
--     print $ assert ("1" == (pass 1 test_input)) "test one-1 passed!"
--     print $ assert ("18" == (pass 2 test_input)) "test one-2 passed!"
--     print $ assert ("18f" == (pass 3 test_input)) "test one-3 passed!"
--     print $ assert ("18f4" == (pass 4 test_input)) "test one-4 passed!"
--     print $ assert ("18f47" == (pass 5 test_input)) "test one-5 passed!"
--     print $ assert ("18f47a" == (pass 6 test_input)) "test one-6 passed!"
--     print $ assert ("18f47a3" == (pass 7 test_input)) "test one-7 passed!"
--     print $ assert ("18f47a30" == (pass 8 test_input)) "test one-8 passed!"
--     print $ assert ("18f47a30" == (part_one test_input)) "test one passed!"
    print $ assert ("f97c354d" == (part_one input)) "part one passed!"
--     let h = part_two test_input
--     print h
--     print $ assert ("05ace8e3" == h) "test two passed!"
    let h = part_two input
    print h
    print $ assert ("863dde27" == h) "part two passed!"
