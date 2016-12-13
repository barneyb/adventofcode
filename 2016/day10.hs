import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Map as M
import Utils

type Value = Int

type Id = Int

data Cmd = Load Value Id | Pass Id Id Id | Dump Id Id Id | PassDump Id Id Id | DumpPass Id Id Id deriving (Eq, Show)

load_regex :: String
load_regex = "value ([0-9]+) goes to bot ([0-9]+)"

pass_regex :: String
pass_regex = "bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (bot|output) ([0-9]+)"

parse :: String -> Cmd
parse s
    | "value" `L.isPrefixOf` s =
        let [v, b] = map read (regexgrps s load_regex)
        in Load v b
    | otherwise                =
        let ps = regexgrps s pass_regex
            ds = (ps!!1, ps!!3)
            is = map read [ps!!0, ps!!2, ps!!4]
        in p ds is
        where
            p :: (String, String) -> [Int] -> Cmd
            p ("bot", "bot") [a,x,y] = Pass a x y
            p ("bot", "output") [a,x,y] = PassDump a x y
            p ("output", "bot") [a,x,y] = DumpPass a x y
            p ("output", "output") [a,x,y] = Dump a x y

part_one :: String -> Int
part_one input = length input

--part_two :: String -> Int
--part_two input = length input

test_input =
    "value 5 goes to bot 2\n\
    \bot 2 gives low to bot 1 and high to bot 0\n\
    \value 3 goes to bot 1\n\
    \bot 1 gives low to output 1 and high to bot 0\n\
    \bot 0 gives low to output 2 and high to output 0\n\
    \value 2 goes to bot 2"

main = do
    input <- readFile "day10_input.txt"

    print $ parse "value 0 goes to bot 2"
    print $ parse "bot 3 gives low to bot 4 and high to bot 5"
    print $ parse "bot 6 gives low to output 7 and high to bot 8"
    print $ parse "bot 9 gives low to output 10 and high to output 11"
    print $ parse "bot 12 gives low to bot 13 and high to output 14"

    print $ map parse (lines test_input)
    print $ map parse (lines input)

    let r = part_one input
    print r
    print $ assert (0 == r) "part one passed!"
--    let r = part_two input
--    print r
--    print $ assert (0 == r) "part two passed!"
