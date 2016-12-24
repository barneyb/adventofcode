{-# LANGUAGE DeriveGeneric #-}

import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Set as Set
import qualified Data.HashSet as S
import Data.Hashable
import GHC.Generics (Generic)
import qualified Data.Map.Strict as M
import Debug.Trace
import Utils

data Element = Thulium
             | Plutonium
             | Strontium
             | Promethium
             | Ruthenium
             | Elerium
             | Dilithium
             deriving (Eq, Ord, Bounded, Enum, Show, Generic)

instance Hashable Element

data Type = Generator
          | Microchip
          deriving (Eq, Ord, Enum, Show, Generic)

instance Hashable Type

data Floor = First
           | Second
           | Third
           | Fourth
           deriving (Eq, Ord, Bounded, Enum, Show, Generic)

instance Hashable Floor

type Item = (Element, Type)

type Items = S.HashSet Item

type ItemMap = M.Map Floor Items

data World = World { elevator     :: Floor
                   , itemsByFloor :: ItemMap
                   } deriving (Eq, Show)

instance Hashable World where
    hashWithSalt salt w = M.foldlWithKey
        (\h f' is ->
            (S.foldl' (\s i -> hashWithSalt s i) (hashWithSalt h f') is))
        (hashWithSalt salt (elevator w))
        (itemsByFloor w)

type WorldSet = S.HashSet World

type Generation = (Int, [World])

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
            | S.member (e, t) (items w f) = head (show e) : [head (show t)]
            | otherwise                   = ". "

hashWorld :: World -> Int
hashWorld = hashWithSalt 0

is_valid_items :: Items -> Bool
is_valid_items is =
    let gs = S.map fst $ S.filter ((== Generator) . snd) is
        ms = S.map fst $ S.filter ((== Microchip) . snd) is
    in (S.null gs) || (S.null ms) || (S.null (S.difference ms gs))

is_valid_world :: World -> Bool
is_valid_world w = all is_valid_items (M.elems (itemsByFloor w))

is_complete :: World -> Bool
is_complete w =
    let f1 = items w First
        f2 = items w Second
        f3 = items w Third
    in all S.null [f1, f2, f3]

items :: World -> Floor -> Items
items w f =
    let Just is = M.lookup f (itemsByFloor w)
    in is

derive :: World -> [World]
derive w = map (\(tf, (f, is), (f', is')) ->
    from_el_items tf $ M.insert f is (M.insert f' is' (itemsByFloor w))) (mods w)

mods :: World -> [(Floor, (Floor, Items), (Floor, Items))]
mods w =
    let e = elevator w
        is = S.toList $ items w e
        one_splits = get_splits is
        splits = one_splits ++ (split_again one_splits)
        tfs = neighbors e
    in concat $
        map (\tf ->
            map (\s ->
                (tf, (e, S.fromList (snd s)), (tf, S.union (items w tf) (S.fromList (fst s))))) splits) tfs

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

next_gen :: (Generation, WorldSet) -> Int -> (Generation, WorldSet)
next_gen ((_, ws), aws) n =
    let drvd = concat $ map derive ws
        valid = filter is_valid_world drvd
        (ws', aws') = L.foldl f ([], aws) valid
        l xs = show $ length xs
    in trace ("gen " ++ (show n) ++ ": derived " ++ (l drvd) ++ " with " ++ (l valid) ++ " valid and " ++ (l ws') ++ " new (" ++ (show $ S.size aws') ++ " total)") ((n, ws'), aws')
    where
        f :: ([World], WorldSet) -> World -> ([World], WorldSet)
        f (ws, aws) w
            | S.member w aws = (ws, aws)
            | otherwise      = (w:ws, S.insert w aws)

check_gen :: Generation -> Bool
check_gen (n, ws) =
--     if null ws then
--         error ("generation " ++ show n ++ " is empty")
--     else if length ws > 400000 then
--         error ("generation " ++ show n ++ " is too big: " ++ (show (length ws)))
--     else
        all (not . is_complete) ws

from_el_items :: Floor -> ItemMap -> World
from_el_items f m = World { elevator=f
                          , itemsByFloor=m
                          }

from_items :: ItemMap -> World
from_items = from_el_items First

part_one :: ItemMap -> Int
part_one input =
    let w = from_items input
    in fst $ head $ dropWhile check_gen (world_factory w)

--part_two :: String -> Int
--part_two input = length input

test_input = M.fromList [ (First , S.fromList [ (Thulium, Microchip), (Plutonium , Microchip) ])
                        , (Second, S.singleton (Thulium , Generator))
                        , (Third , S.singleton (Plutonium , Generator))
                        , (Fourth, S.empty)
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

    let input = M.fromList [ (First , S.fromList [ (Thulium, Generator), (Thulium, Microchip), (Plutonium, Generator), (Strontium , Generator)])
                           , (Second, S.fromList [ (Plutonium, Microchip), (Strontium , Microchip)])
                           , (Third , S.fromList [ (Promethium, Generator), (Promethium, Microchip), (Ruthenium, Generator), (Ruthenium , Microchip)])
                           , (Fourth, S.empty)
                           ]

    print "part one:"
    prints $ draw (from_items input)

    let r = part_one input
    print ((show r) ++ " generations")
    print $ assert (31 == r) "part one passed!"

    let input2 = M.insertWith S.union First (S.fromList [ (Elerium, Generator), (Elerium, Microchip), (Dilithium, Generator), (Dilithium, Microchip) ]) input

    print "part two:"
    prints $ draw (from_items input2)

    let r = part_one $ input2
    print r
    print $ assert (55 == r) "part two passed!"
