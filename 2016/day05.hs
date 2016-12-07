import Control.Exception (assert)
import Crypto.Hash
import qualified Data.ByteString.Char8 as C

-- taken from 2015's day 4
md5 :: String -> String
md5 s = show (hash (C.pack s) :: Digest MD5)

-- taken from 2015's day 4 (and renamed from chash)
nhash :: String -> Int -> String
nhash key n = md5 (key ++ (show n))

pass :: Int -> String -> String
pass len key =
    let hs = map (nhash key) [0..]
        chs = filter (\ h -> "00000" == (take 5 h)) hs
        cs = map (!! 5) chs
    in take len cs

part_one :: String -> String
part_one = pass 8

--part_two :: String -> String
--part_two input = length input

test_input = "abc"
input = "reyedfim"

main = do
    print $ assert ("1" == (pass 1 test_input)) "test one-1 passed!"
    print $ assert ("18" == (pass 2 test_input)) "test one-2 passed!"
    print $ assert ("18f" == (pass 3 test_input)) "test one-3 passed!"
    print $ assert ("18f4" == (pass 4 test_input)) "test one-4 passed!"
    print $ assert ("18f47" == (pass 5 test_input)) "test one-5 passed!"
    print $ assert ("18f47a" == (pass 6 test_input)) "test one-6 passed!"
    print $ assert ("18f47a3" == (pass 7 test_input)) "test one-7 passed!"
    print $ assert ("18f47a30" == (pass 8 test_input)) "test one-8 passed!"
    print $ assert ("18f47a30" == (part_one test_input)) "test one passed!"
    print $ assert ("f97c354d" == (part_one input)) "part one passed!"
--    print (part_two input)
--    print $ assert (0 == (part_two input)) "part one passed!"
