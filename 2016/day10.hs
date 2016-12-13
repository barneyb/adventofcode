import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Map as M
import Utils

type Value = Int

type Id = Int

data Cmd = Load Value Id | Pass Id Id Id | PassDump Id Id Id | DumpPass Id Id Id | Dump Id Id Id deriving (Eq, Show, Ord)

parse :: String -> Cmd
parse s
    | "value" `L.isPrefixOf` s =
        let [v, b] = map read (regexgrps s "value ([0-9]+) goes to bot ([0-9]+)")
        in Load v b
    | otherwise                =
        let ps = regexgrps s "bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (bot|output) ([0-9]+)"
            ds = (ps!!1, ps!!3)
            is = map read [ps!!0, ps!!2, ps!!4]
        in p ds is
        where
            p :: (String, String) -> [Int] -> Cmd
            p ("bot", "bot") [a,x,y] = Pass a x y
            p ("bot", "output") [a,x,y] = PassDump a x y
            p ("output", "bot") [a,x,y] = DumpPass a x y
            p ("output", "output") [a,x,y] = Dump a x y

loaded_into :: [Cmd] -> Value -> Id
loaded_into cs v =
    let Just c = lookup v $ map (\(Load v' i) -> (v', i)) cs
    in c

pLoadMove :: [Cmd] -> ([Cmd], [Cmd])
pLoadMove = L.partition p
    where
        p :: Cmd -> Bool
        p (Load _ _) = True
        p _ = False

part_one :: String -> Int -> Int -> Int
part_one input l h =
    let (ls, ps) = pLoadMove $ map parse (lines input)
    in loaded_into ls l

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

    let r = parse "value 0 goes to bot 2"
    print r
    print $ assert (Load 0 2 == r) "parse load passed"

    let r = parse "bot 3 gives low to bot 4 and high to bot 5"
    print r
    print $ assert (Pass 3 4 5 == r) "parse pass passed"

    let r = parse "bot 6 gives low to output 7 and high to bot 8"
    print r
    print $ assert (DumpPass 6 7 8 == r) "parse dump pass passed"

    let r = parse "bot 9 gives low to output 10 and high to output 11"
    print r
    print $ assert (Dump 9 10 11 == r) "parse dump passed"

    let r = parse "bot 12 gives low to bot 13 and high to output 14"
    print r
    print $ assert (PassDump 12 13 14 == r) "parse pass dump passed"

    let r = part_one test_input 2 5
    print r
    print $ assert (2 == r) "test one passed!"

    let r = part_one test_input 2 3
    print r
    print $ assert (1 == r) "test two passed!"

--     let r = part_one input 17 61
--     print r
--     print $ assert (0 == r) "part one passed!"

--    let r = part_two input
--    print r
--    print $ assert (0 == r) "part two passed!"
