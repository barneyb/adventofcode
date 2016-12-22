import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Map.Strict as M

data Element = Thulium | Plutonium | Strontium | Promethium | Ruthenium | Hydrogen | Lithium deriving (Eq, Ord, Show)

data Item = Generator Element | Microchip Element deriving (Eq, Ord, Show)

data Floor = First | Second | Third | Fourth deriving (Eq, Ord, Enum, Show)

type ItemMap = M.Map Floor [Item]

data World = World { elevator :: Floor
                   , itemsByFloor :: ItemMap
                   } deriving (Eq, Show)

type Generation = (Int, [World])

is_generator :: Item -> Bool
is_generator (Generator _) = True
is_generator (Microchip _) = False

is_microchip :: Item -> Bool
is_microchip (Generator _) = False
is_microchip (Microchip _) = True

is_valid_items :: [Item] -> Bool
is_valid_items is
    | all is_generator is = True
    | all is_microchip is = True
    | otherwise           =
        let gs = map (\(Generator e) -> e) $ filter is_generator is
            ms = map (\(Microchip e) -> e) $ filter is_microchip is
        in all (`elem` gs) ms

is_valid_world :: World -> Bool
is_valid_world w = all is_valid_items (M.elems (itemsByFloor w))

is_complete :: World -> Bool
is_complete w
    | not (is_valid_world w) = False
    | otherwise              =
        let f1 = items w First
            f2 = items w Second
            f3 = items w Third
        in L.all ((== 0) . length) [f1, f2, f3]

items :: World -> Floor -> [Item]
items w f =
    let Just is = M.lookup f (itemsByFloor w)
    in is

derive :: World -> [World]
derive w =
    let f = elevator w
    in map (\(tf, (f, is), (f', is')) ->
        World { elevator = tf
              , itemsByFloor = M.insert f is (M.insert f' is' (itemsByFloor w))
              }) (mods w)

mods :: World -> [(Floor, (Floor, [Item]), (Floor, [Item]))]
mods w =
    let f = elevator w
        is = items w f
        one_splits = get_splits is
        splits = one_splits ++ (split_again one_splits)
        tfs = tgt_floors f
    in concat $
        map (\tf ->
            map (\s ->
                (tf, (f, snd s), (tf, (items w tf) ++ (fst s)))) splits) tfs

get_splits :: Ord a => [a] -> [([a], [a])]
get_splits xs = map (\x -> ([x], L.delete x xs)) xs

split_again :: Ord a => [([a], [a])] -> [([a], [a])]
split_again ss = L.nub $ concat $
    map (\(x, xs) ->
        map (\(y, ys) ->
            (L.sort (x++y), ys)) (get_splits xs)) ss

tgt_floors :: Floor -> [Floor]
tgt_floors First = [Second]
tgt_floors Fourth = [Third]
tgt_floors f = [pred f, succ f]

world_factory :: World -> [Generation]
world_factory w = scanl next_gen (0, [w]) [1..]

next_gen :: Generation -> Int -> Generation
next_gen (_, ws) n = (n, filter is_valid_world (L.nub $ concat $ map derive ws))

check_gen :: Generation -> Bool
check_gen (_, ws) = if length ws == 0
    then error "empty generation before solving..."
    -- 375-380K for the example w/o nub
    -- 425-450 (not K!) for the example w/ nub
    else if length ws > 450
    then error "generation is too big"
    else all (not . is_complete) ws

part_one :: ItemMap -> Int
part_one input =
    let w = World { elevator=First
                  , itemsByFloor=input
                  }
    in fst $ head $ dropWhile check_gen (world_factory w)

--part_two :: String -> Int
--part_two input = length input

test_input = M.fromList [ (First, [ Microchip Hydrogen, Microchip Lithium ])
                        , (Second, [ Generator Hydrogen ])
                        , (Third, [ Generator Lithium ])
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

    4  .   .   .   .   .   .   .   .   .   .
    3  .   .   .   .   .   .   FG  FM  RG  RM
    2  .   .   .   PM  .   SM  .   .   .   .
    1  TG  TM  PG  .   SG  .   .   .   .   .
    -}

--     let input = M.fromList [ (First, [ Generator Thulium, Microchip Thulium, Generator Plutonium, Generator Strontium ])
--                            , (Second, [ Microchip Plutonium, Microchip Strontium ])
--                            , (Third, [ Generator Promethium, Microchip Promethium, Generator Ruthenium, Microchip Ruthenium ])
--                            , (Fourth, [ ])
--                            ]
--
--     let r = part_one input
--     print r
--     print $ assert (0 == r) "part one passed!"

--     let r = part_two input
--     print r
--     print $ assert (0 == r) "part two passed!"
