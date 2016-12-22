import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M

data Element = Thulium | Plutonium | Strontium | Promethium | Ruthenium | Hydrogen | Lithium deriving (Eq, Ord, Show)

data Type = Generator | Microchip deriving (Eq, Ord, Show)

type Item = (Element, Type)

data Floor = First | Second | Third | Fourth deriving (Eq, Ord, Bounded, Enum, Show)

type ItemMap = M.Map Floor [Item]

data World = World { elevator :: Floor
                   , itemsByFloor :: ItemMap
                   } deriving (Eq, Ord, Show)

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
world_factory w = scanl next_gen (0, [w]) [1..]

next_gen :: Generation -> Int -> Generation
next_gen (_, ws) n =
    let ws' = filter is_valid_world (concat $ map derive ws)
        (ws'', _) = foldl f ([], S.empty) ws'
    in (n, ws'')
    where
        f :: ([World], S.Set World) -> World -> ([World], S.Set World)
        f (ws, aws) w
            | S.member w aws = (ws, aws)
            | otherwise      = (w:ws, S.insert w aws)

check_gen :: Generation -> Bool
check_gen (n, ws) = if length ws == 0
    then error ("generation " ++ show n ++ " is empty")
    -- 375-380K for the example w/o nub
    -- 425-450 (not K!) for the example w/ nub
    else if length ws > 100000
    then error ("generation " ++ show n ++ " is too big: " ++ (show $ length ws))
    else all (not . is_complete) ws

part_one :: ItemMap -> Int
part_one input =
    let w = World { elevator=First
                  , itemsByFloor=input
                  }
    in fst $ head $ dropWhile check_gen (world_factory w)

--part_two :: String -> Int
--part_two input = length input

test_input = M.fromList [ (First, [ (Hydrogen, Microchip), (Lithium , Microchip)])
                        , (Second, [ (Hydrogen , Generator)])
                        , (Third, [ (Lithium , Generator)])
                        , (Fourth, [ ])
                        ]
test_world = World { elevator=First
                   , itemsByFloor=test_input
                   }

main = do

    print $ assert (is_valid_world test_world) "is valid world passed"

    let r = part_one test_input
    print r
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

    let r = part_one input
    print r
    print $ assert (0 == r) "part one passed!"

--     let r = part_two input
--     print r
--     print $ assert (0 == r) "part two passed!"
