import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Utils

type Value = Int

type Id = Int

data Cmd = Load Value Id | Pass Id Id Id | PassDump Id Id Id | DumpPass Id Id Id | Dump Id Id Id deriving (Eq, Show)

{-
this doesn't actually help, because the sort algorithm is efficient.
if it were bubble sort, I *think* it'd do the right thing, because
it'd compare every pair, not just enough few enough to get them in
order. I'm not 100% sure though...
-}
instance Ord Cmd where
--     compare :: Cmd -> Cmd -> Ordering
    compare (Load _ i) (Load _ i') = i `compare` i'
    compare (Load _ _) _ = LT
    compare _ (Load _ _) = GT
    compare (Dump _ _ _) _ = GT
    compare _ (Dump _ _ _) = LT
    compare (Pass b l h) (Pass b' l' h')
        | l == b'   = LT
        | h == b'   = LT
        | b == l'   = GT
        | b == h'   = GT
    compare (Pass b l h) (PassDump b' l' _)
        | l == b'   = LT
        | h == b'   = LT
        | b == l'   = GT
    compare (Pass b l h) (DumpPass b' _ h')
        | l == b'   = LT
        | h == b'   = LT
        | b == h'   = GT
    compare (PassDump b l _) (Pass b' l' h')
        | l == b'   = LT
        | b == l'   = GT
        | b == h'   = GT
    compare (PassDump b l _) (PassDump b' l' _)
        | l == b'   = LT
        | b == l'   = GT
    compare (PassDump b l _) (DumpPass b' _ h')
        | l == b'   = LT
        | b == h'   = GT
    compare (DumpPass b _ h) (Pass b' l' h')
        | h == b'   = LT
        | b == l'   = GT
        | b == h'   = GT
    compare (DumpPass b _ h) (PassDump b' l' _)
        | h == b'   = LT
        | b == l'   = GT
    compare (DumpPass b _ h) (DumpPass b' _ h')
        | h == b'   = LT
        | b == h'   = GT
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

parse_tests :: IO ()
parse_tests = do
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

part_one :: String -> Int -> Int -> Int
part_one input l h =
    let cs = L.sort $ map parse (lines input)
        vals = foldl scn M.empty cs
    in fst $ head $ filter f (M.toList vals)
    where
        f :: (Id, (Value, Maybe Value)) -> Bool
        f (_, (l', Just h')) = l == l' && h == h'
        f (_, (_, Nothing)) = False -- for bots that never got their second value

        merge :: (Value, Maybe Value) -> (Value, Maybe Value) -> (Value, Maybe Value)
        merge (v, Nothing) (v', Nothing)
            | v < v'    = (v, Just v')
            | otherwise = (v', Just v)

        get_low :: Id -> M.Map Id (Value, Maybe Value) -> Value
        get_low i a
            | M.member i a =
                let (Just (v, _)) = M.lookup i a
                in v
            | otherwise    = error ("ain't no " ++ (show i) ++ " in " ++ (show a))

        get_high :: Id -> M.Map Id (Value, Maybe Value) -> Value
        get_high i a
            | M.member i a = rip (M.lookup i a)
            | otherwise    = error ("ain't no " ++ (show i) ++ " in " ++ (show a))
            where
                rip :: Maybe (Value, Maybe Value) -> Value
                rip (Just (_, Just v)) = v
                rip (Just (_, Nothing)) = error ("no high for " ++ (show i) ++ " in " ++ (show a))

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

    parse_tests

    let r = part_one test_input 2 5
    print r
    print $ assert (2 == r) "test one passed!"

    let r = part_one test_input 2 3
    print r
    print $ assert (1 == r) "test two passed!"

    let r = part_one input 17 61
    print r
    print $ assert (0 == r) "part one passed!"

--    let r = part_two input
--    print r
--    print $ assert (0 == r) "part two passed!"
