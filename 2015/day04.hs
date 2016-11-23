import Control.Exception (assert)
import Crypto.Hash
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import qualified Data.List as L

md5 :: String -> String
md5 s = show (hash (C.pack s) :: Digest MD5)

chash :: String -> Int -> String
chash key n = md5 (key ++ (show n))

is_coin :: String -> String -> Bool
is_coin prefix coin = prefix == (take (length prefix) coin)

coin :: String -> String -> Int
coin key prefix =
    let (Just i) = L.findIndex (\n -> is_coin prefix (chash key n)) [1..]
    in i + 1

main = do
    input <- readFile "day03_input.txt"
    print $ assert ("000001dbbfa" == (take 11 (chash "abcdef" 609043))) "chash checks out"
    print $ assert (is_coin "00000" "000001dbbfa") "is_coin checks out"
    print $ assert (609043 == (coin "abcdef" "00000")) "test one passed"
    print $ assert (1048970 == (coin "pqrstuv" "00000")) "test two passed"
    print $ assert (117946 == (coin "ckczppom" "00000")) "part one passed"
    print $ assert (3938038 == (coin "ckczppom" "000000")) "part two passed"
