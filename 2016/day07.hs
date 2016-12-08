import Control.Exception (assert)
import qualified Data.List as L

data IP = IP {
    nets :: [String],
    hypernets :: [String]
} deriving (Eq, Show)

parts :: String -> [String]
parts s
    | '[' `elem` s =
        let (p, r) = L.span (/= '[') s
            (e, r') = L.span (/= ']') (tail r)
        in p:e:(parts (tail r'))
    | otherwise    = [s]

parse :: String -> IP
parse s =
    let ps = zip [1..] (parts s)
        ns = map snd (filter (odd . fst) ps)
        hns = map snd (filter (even . fst) ps)
    in IP{nets=ns, hypernets=hns}

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

abba :: String -> Bool
abba [] = False
abba [_] = False
abba [_, _] = False
abba [_, _, _] = False
abba (x:y:y':x':xs) = (x /= y && x == x' && y == y') || abba (y:y':x':xs)

tls :: IP -> Bool
tls ip = (any abba (nets ip)) && (not (any abba (hypernets ip)))

part_one :: String -> Int
part_one input = length $ filter tls (map parse (lines input))

--part_two :: String -> Int
--part_two input = length input

test_strings = [
    "abba[mnop]qrst",
    "abcd[bddb]xyyx",
    "aaaa[qwer]tyui",
    "ioxxoj[asdfgh]zxcvbn",
    "drink[some]super[delicious]magma",
    "ektijwczwnlancuqfv[luqhtfgwmlilhwnk]gxgivxlnerdhbhetfz[bzczfdorrsptzikjmct]mfrsvxgxijtusmvjd[sbpnwycbrykuhsinudc]bmpikuskzlxcoidp",
    "asdf[asdf]asdf[asdf]asdfabbaasdf[asdf]asdf",
    "asdf[asdf]asdf[asdf]asdfabbaasdf[asdfxooxasdf]asdf"
    ]
test_ips = [
    IP {nets = ["abba", "qrst"], hypernets = ["mnop"]},
    IP {nets = ["abcd", "xyyx"], hypernets = ["bddb"]},
    IP {nets = ["aaaa", "tyui"], hypernets = ["qwer"]},
    IP {nets = ["ioxxoj", "zxcvbn"], hypernets = ["asdfgh"]},
    IP {nets = ["drink", "super", "magma"], hypernets = ["some", "delicious"]},
    IP {nets = ["ektijwczwnlancuqfv", "gxgivxlnerdhbhetfz", "mfrsvxgxijtusmvjd", "bmpikuskzlxcoidp"], hypernets = ["luqhtfgwmlilhwnk", "bzczfdorrsptzikjmct", "sbpnwycbrykuhsinudc"]},
    IP {nets = ["asdf", "asdf", "asdfabbaasdf", "asdf"], hypernets = ["asdf", "asdf", "asdf"]},
    IP {nets = ["asdf", "asdf", "asdfabbaasdf", "asdf"], hypernets = ["asdf", "asdf", "asdfxooxasdf"]}
    ]
test_tlss = [
    True,
    False,
    False,
    True,
    False,
    False,
    True,
    False
    ]

main = do
    input <- readFile "day07_input.txt"
    print $ assert (abba "abba") "abba passed"
    print $ assert (abba "ioxxoj") "ioxxoj passed"
    print $ assert (not (abba "aaaa")) "aaaa passed"
    print $ assert (not (abba "drink")) "drink passed"
    print $ assert (not (abba "abxba")) "abxba passed"
    foldl1 (>>) (map (\p -> print p >> (print $ assert (fst p == snd p) "parse worked")) (zip (map parse test_strings) test_ips))
    foldl1 (>>) (map (\p -> print p >> (print $ assert (fst p == snd p) "tls worked")) (zip (map tls test_ips) test_tlss))
    let r = part_one input
    print r
    print $ assert (110 == r) "part one passed!"
--    let r = part_two input
--    print r
--    print $ assert (0 == r) "part two passed!"
