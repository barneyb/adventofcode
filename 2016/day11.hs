import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Debug.Trace
import Utils

data Element = Thulium | Plutonium | Strontium | Promethium | Ruthenium deriving (Eq, Ord, Bounded, Enum, Show)

data Type = Generator | Microchip deriving (Eq, Ord, Enum, Show)

type Item = (Element, Type)

data Floor = First | Second | Third | Fourth deriving (Eq, Ord, Bounded, Enum, Show)

type ItemMap = M.Map Floor [Item]

data World = World { elevator :: Floor
                   , itemsByFloor :: ItemMap
                   } deriving (Eq, Ord, Show)

draw :: World -> [String]
draw w = map (\f ->
    unwords $ (elv f) : (map (el f) [minBound..])) (reverse [minBound..])
    where
        elv :: Floor -> String
        elv f = if elevator w == f then "E" else "."

        el :: Floor -> Element -> String
        el f e =
            let p = elp f e
            in unwords [p Generator, p Microchip]

        elp :: Floor -> Element -> Type -> String
        elp f e t
            | (e, t) `elem` (items w f) = head (show e) : [head (show t)]
            | otherwise                 = ". "

type Generation = (Int, [World])

is_valid_items :: [Item] -> Bool
is_valid_items is =
    let gs = map fst $ filter ((== Generator) . snd) is
        ms = map fst $ filter ((== Microchip) . snd) is
    in (null gs) || (null ms) || (all (`elem` gs) ms)

is_valid_world :: World -> Bool
is_valid_world w = all is_valid_items (M.elems (itemsByFloor w))

is_complete :: World -> Bool
is_complete w =
    let f1 = items w First
        f2 = items w Second
        f3 = items w Third
    in L.all null [f1, f2, f3]

items :: World -> Floor -> [Item]
items w f =
    let Just is = M.lookup f (itemsByFloor w)
    in is

derive :: World -> [World]
derive w = map (\(tf, (f, is), (f', is')) ->
        World { elevator = tf
              , itemsByFloor = M.insert f is (M.insert f' is' (itemsByFloor w))
              }) (mods w)

mods :: World -> [(Floor, (Floor, [Item]), (Floor, [Item]))]
mods w =
    let e = elevator w
        is = items w e
        one_splits = get_splits is
        splits = one_splits ++ (split_again one_splits)
        tfs = neighbors e
    in concat $
        map (\tf ->
            map (\s ->
                (tf, (e, snd s), (tf, (items w tf) ++ (fst s)))) splits) tfs

get_splits :: Ord a => [a] -> [([a], [a])]
get_splits xs = map (\x -> ([x], L.delete x xs)) xs

split_again :: Ord a => [([a], [a])] -> [([a], [a])]
split_again ss = L.nub $ concat $
    map (\(x, xs) ->
        map (\(y, ys) ->
            (L.sort (x++y), ys)) (get_splits xs)) ss

neighbors :: (Eq a, Bounded a, Enum a) => a -> [a]
neighbors f
    | f == minBound = [succ f]
    | f == maxBound = [pred f]
    | otherwise     = [pred f, succ f]

world_factory :: World -> [Generation]
world_factory w = map fst $ L.scanl next_gen ((0, [w]), S.empty) [1..]

next_gen :: (Generation, S.Set World) -> Int -> (Generation, S.Set World)
next_gen ((_, ws), aws) n =
    let drvd = concat $ map derive ws
        valid = filter is_valid_world drvd
        (ws', aws') = L.foldl' f ([], aws) valid
        l xs = show $ length xs
    in trace ("gen " ++ (show n) ++ ": from " ++ (l ws) ++ " derive " ++ (l drvd) ++ " with " ++ (l valid) ++ " valid and " ++ (l ws') ++ " new") ((n, ws'), aws')
    where
        f :: ([World], S.Set World) -> World -> ([World], S.Set World)
        f (ws, aws) w
            | S.member w aws = (ws, aws)
            | otherwise      = (w:ws, S.insert w aws)

check_gen :: Generation -> Bool
check_gen (n, ws) =
    let l = length ws
    in if null ws
        then error ("generation " ++ show n ++ " is empty")
        -- 375-380K for the example w/o nub
        -- 425-450 (not K!) for the example w/ nub
        else if l > 100000
        then error ("generation " ++ show n ++ " is too big: " ++ (show l))
        else all (not . is_complete) ws

from_items :: ItemMap -> World
from_items m = World { elevator=First
                     , itemsByFloor=m
                     }

part_one :: ItemMap -> Int
part_one input =
    let w = from_items input
    in fst $ head $ dropWhile check_gen (world_factory w)

--part_two :: String -> Int
--part_two input = length input

test_input = M.fromList [ (First, [ (Thulium, Microchip), (Plutonium , Microchip)])
                        , (Second, [ (Thulium , Generator)])
                        , (Third, [ (Plutonium , Generator)])
                        , (Fourth, [ ])
                        ]
test_world = from_items test_input

main = do

    print $ assert (is_valid_world test_world) "is valid world passed"

    print "example:"
    prints $ draw test_world

    let r = part_one test_input
    print ((show r) ++ " generations")
    print $ assert (11 == r) "test one passed!"

    {-
    The first floor contains a thulium generator, a thulium-compatible microchip, a plutonium generator, and a strontium generator.
    The second floor contains a plutonium-compatible microchip and a strontium-compatible microchip.
    The third floor contains a promethium generator, a promethium-compatible microchip, a ruthenium generator, and a ruthenium-compatible microchip.
    The fourth floor contains nothing relevant.

    .  .   .   .   .   .   .   .   .   .   .
    .  .   .   .   .   .   .   FG  FM  RG  RM
    .  .   .   .   PM  .   SM  .   .   .   .
    E  TG  TM  PG  .   SG  .   .   .   .   .
    -}

    let input = M.fromList [ (First, [ (Thulium, Generator), (Thulium, Microchip), (Plutonium, Generator), (Strontium , Generator)])
                           , (Second, [ (Plutonium, Microchip), (Strontium , Microchip)])
                           , (Third, [ (Promethium, Generator), (Promethium, Microchip), (Ruthenium, Generator), (Ruthenium , Microchip)])
                           , (Fourth, [ ])
                           ]

    print "puzzle:"
    prints $ draw (from_items input)

    let r = part_one input
    print ((show r) ++ " generations")
    print $ assert (0 == r) "part one passed!"

--     let r = part_two input
--     print r
--     print $ assert (0 == r) "part two passed!"
