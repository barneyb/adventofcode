import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Utils

type Value = Int

type Id = Int

data Cmd = Load Value Id | Pass Id Id Id | PassDump Id Id Id | DumpPass Id Id Id | Dump Id Id Id deriving (Eq, Show)

instance Ord Cmd where
    compare (Load _ i) (Load _ i') = i `compare` i'
    compare (Load _ _) _ = LT
    compare _ (Load _ _) = GT
    compare (Dump _ _ _) _ = GT
    compare _ (Dump _ _ _) = LT
    compare (Pass _ l h) (Pass b _ _)
        | l == b    = LT
        | h == b    = LT
    compare (Pass _ l h) (PassDump b _ _)
        | l == b    = LT
        | h == b    = LT
    compare (Pass _ l h) (DumpPass b _ _)
        | l == b    = LT
        | h == b    = LT
    compare (PassDump _ l _) (Pass b _ _)
        | l == b    = LT
    compare (PassDump _ l _) (PassDump b _ _)
        | l == b    = LT
    compare (PassDump _ l _) (DumpPass b _ _)
        | l == b    = LT
    compare (DumpPass _ _ h) (Pass b _ _)
        | h == b    = LT
    compare (DumpPass _ _ h) (PassDump b _ _)
        | h == b    = LT
    compare (DumpPass _ _ h) (DumpPass b _ _)
        | h == b    = LT
    compare _ _ = EQ

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

part_one :: String -> Int -> Int -> Int
part_one input l h =
    let cs = L.sort $ map parse (lines input)
        vals = foldl scn M.empty cs
    in fst $ head $ filter f (M.toList vals)
    where
        f :: (Id, (Value, Maybe Value)) -> Bool
        f (_, (l', Just h')) = l == l' && h == h'
        f _ = False -- for bots that never got their second value
        merge :: (Value, Maybe Value) -> (Value, Maybe Value) -> (Value, Maybe Value)
        merge (v, Nothing) (v', Nothing)
            | v < v'    = (v, Just v')
            | otherwise = (v', Just v)
        get_low :: Id -> M.Map Id (Value, Maybe Value) -> Value
        get_low i a =
            let (Just (v, _)) = M.lookup i a
            in v
        get_high :: Id -> M.Map Id (Value, Maybe Value) -> Value
        get_high i a =
            let (Just (_, Just v)) = M.lookup i a
            in v
        scn :: M.Map Id (Value, Maybe Value) -> Cmd -> M.Map Id (Value, Maybe Value)
        scn a (Load v i) = M.insertWith merge i (v, Nothing) a
        scn a (Pass i l h) = M.insertWith merge h (get_high i a, Nothing) (M.insertWith merge l (get_low i a, Nothing) a)
        scn a (DumpPass i _ h) = M.insertWith merge h (get_high i a, Nothing) a
        scn a (PassDump i l _) = M.insertWith merge l (get_low i a, Nothing) a
        scn a (Dump _ _ _) = a -- ignore

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
