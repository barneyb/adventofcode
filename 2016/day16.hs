import Control.Exception (assert)

chksum :: String -> String
chksum s =
    let c = f s
    in if even $ length c
        then chksum c
        else c
    where
        f :: String -> String
        f [] = []
        f (x:y:xs)
            | x == y    = '1' : (f xs)
            | otherwise = '0' : (f xs)

dragon :: String -> String
dragon a =
    let b = map (\c -> if c == '1' then '0' else '1') (reverse a)
    in a ++ ('0':b)

fill :: String -> Int -> String
fill seed l =
    let gens = dropWhile ((< l) . length) (scanl (\s _ -> dragon s) seed [1..])
    in take l (head gens)

part_one :: String -> Int -> String
part_one input l = chksum (fill input l)

--part_two :: String -> Int
--part_two input = length input

test_input = "10000"

main = do
    let input = "10001001100000001"

    print $ assert ((dragon "1") == "100") "dragon one"
    print $ assert ((dragon "0") == "001") "dragon two"
    print $ assert ((dragon "111100001010") == "1111000010100101011110000") "dragon three"

    let r = fill test_input 20
    print r
    print $ assert ("10000011110010000111" == r) "example one A passed!"

    let r = chksum "10000011110010000111"
    print r
    print $ assert ("01100" == r) "example one B passed!"

    let r = part_one test_input 20
    print r
    print $ assert ("01100" == r) "example one passed!"

    let r = part_one input 272
    print r
    print $ assert ("10101001010100001" == r) "part one passed!"

--     let r = part_two input
--     print r
--     print $ assert (0 == r) "part two passed!"
