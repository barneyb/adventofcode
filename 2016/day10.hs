import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Utils

type Val = Int

type Id = Int

data Sink = Bot Id Val Val | Bin Id Val deriving (Show, Eq)

data Cmd = Get Val Msg | Pass Id Msg Msg deriving (Show, Eq)

data Msg = ToBot Id | ToBin Id deriving (Show, Eq)

parse :: String -> Cmd
parse s
    | "value" `L.isPrefixOf` s =
        let [v, b] = map read (regexgrps s "value ([0-9]+) goes to bot ([0-9]+)")
        in Get v (ToBot b)
    | otherwise                =
        let ps = regexgrps s "bot ([0-9]+) gives low to (bot|output) ([0-9]+) and high to (bot|output) ([0-9]+)"
            ds = [ps!!1, ps!!3]
            is = map read [ps!!0, ps!!2, ps!!4]
            ms = map to_msg (zip ds (tail is))
        in Pass (head is) (ms!!0) (ms!!1)
        where
            to_msg :: (String, Int) -> Msg
            to_msg ("bot", i) = ToBot i
            to_msg ("output", i) = ToBin i

parse_file :: String -> [Cmd]
parse_file input = map parse (lines input)

new_bot :: Id -> Val -> Val -> Sink
new_bot i x y
    | x < y     = Bot i x y
    | otherwise = Bot i y x

-- admittedly pointless, but for consistency
new_bin :: Id -> Val -> Sink
new_bin i x  = Bin i x

load :: (M.Map Id Sink, M.Map Id Val) -> Val -> Msg -> (M.Map Id Sink, M.Map Id Val)
load (ss, st) v (ToBot i)
    | M.member i st = (M.insert i (new_bot i v (st M.! i)) ss, M.delete i st)
    | otherwise     = (ss, M.insert i v st)
load (ss, st) v (ToBin i) = (M.insert (1000 + i) (new_bin i v) ss, st)

sinks :: [Cmd] -> [Sink]
sinks cmds =
    let
        gen_factory = scanl next_gen (M.empty, M.empty, cmds) [1..]
        gens = dropWhile (\(_, _, cs) -> length cs > 0) gen_factory
        (ss, _, _) = head gens
    in M.elems ss
    where
        next_gen :: (M.Map Id Sink, M.Map Id Val, [Cmd]) -> Int -> (M.Map Id Sink, M.Map Id Val, [Cmd])
        next_gen a@(sinks, temp, cmds) _ = foldl do_cmd (sinks, temp, []) cmds

        do_cmd :: (M.Map Id Sink, M.Map Id Val, [Cmd]) -> Cmd -> (M.Map Id Sink, M.Map Id Val, [Cmd])
        do_cmd (ss, st, cs) (Get v m) =
            let (ss', st') = load (ss, st) v m
            in (ss', st', cs)
        do_cmd (ss, st, cs) c@(Pass i ml mh)
            | M.member i ss =
                let (Bot _ l h) = ss M.! i
                    (ss', st') = load (ss, st) l ml
                    (ss'', st'') = load (ss', st') h mh
                in (ss'', st'', cs)
            | otherwise     = (ss, st, c:cs)

which_bot_compares :: Int -> Int -> [Sink] -> Int
which_bot_compares l h ss =
    let Just (Bot i _ _) = L.find pred ss
    in i
    where
        pred :: Sink -> Bool
        pred (Bot _ l' h') = l == l' && h == h'
        pred (Bin _ _) = False

get_bins :: [Id] -> [Sink] -> [Val]
get_bins is = map (\(Bin _ v) -> v) . filter (f is)
    where
        f :: [Id] -> Sink -> Bool
        f is (Bot _ _ _) = False
        f is (Bin i _) = i `elem` is

part_one :: String -> Int
part_one input = which_bot_compares 17 61 (sinks (parse_file input))

part_two :: String -> Int
part_two input = product $ get_bins [0, 1, 2] (sinks (parse_file input))

test_input =
    "value 5 goes to bot 2\n\
    \bot 2 gives low to bot 1 and high to bot 0\n\
    \value 3 goes to bot 1\n\
    \bot 1 gives low to output 1 and high to bot 0\n\
    \bot 0 gives low to output 2 and high to output 0\n\
    \value 2 goes to bot 2"

test_cmds = [ Get 5 (ToBot 2)
            , Pass 2 (ToBot 1) (ToBot 0)
            , Get 3 (ToBot 1)
            , Pass 1 (ToBin 1) (ToBot 0)
            , Pass 0 (ToBin 2) (ToBin 0)
            , Get 2 (ToBot 2)
            ]

test_gens = [ Bot 0 3 5
            , Bot 1 2 3
            , Bot 2 2 5
            , Bin 0 5
            , Bin 1 2
            , Bin 2 3
            ]

main = do
    input <- readFile "day10_input.txt"

    let r = which_bot_compares 2 5 test_gens
    print r
    print $ assert (2 == r) "test one passed!"

    let r = which_bot_compares 2 3 test_gens
    print r
    print $ assert (1 == r) "test two passed!"

    let r = sinks test_cmds
    print "generations:"
    foldl1 (>>) (map print r)
    print $ assert (test_gens == r) "generations passed"

    let r = parse_file test_input
    print r
    print $ assert (test_cmds == r) "parse test passed!"

    let r = part_one input
    print r
    print $ assert (73 == r) "part one passed!"

    let r = part_two input
    print r
    print $ assert (3965 == r) "part two passed!"
