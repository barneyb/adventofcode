import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Map.Strict as M

data Element = Thulium | Plutonium | Strontium | Promethium | Ruthenium | Hydrogen | Lithium deriving (Eq, Show)

data Item = Generator Element | Microchip Element deriving (Eq, Show)

data Floor = First | Second | Third | Fourth deriving (Eq, Ord, Enum, Show)

type ItemMap = M.Map Floor [Item]

data World = World { elevator :: Floor
                   , itemsByFloor :: ItemMap
                   , generation :: Int
                   } deriving (Eq, Show)

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
        is = items w f
        sss = filter ((== 2) . length) (L.subsequences is)
    in []

part_one :: ItemMap -> Int
part_one input =
    let w = World { elevator=First
                  , itemsByFloor=input
                  , generation=0
                  }
        world_factory = scanl next_gen [w] [1..]
        gen = head $ dropWhile check_gen world_factory
    in generation $ head gen
    where
        next_gen :: [World] -> Int -> [World]
        next_gen ws _ = filter is_valid_world (concat $ map derive ws)

        check_gen :: [World] -> Bool
        check_gen ws = if length ws == 0
            then error "empty generation before solving..."
            else all (not . is_complete) ws

--part_two :: String -> Int
--part_two input = length input

main = do
    let test_input = M.fromList [ (First, [ Microchip Hydrogen, Microchip Lithium ])
                                , (Second, [ Generator Hydrogen ])
                                , (Third, [ Generator Lithium ])
                                , (Fourth, [ ])
                                ]

    print $ assert (is_valid_world World { elevator=First
                                         , itemsByFloor=test_input
                                         , generation=0
                                         }) "is valid world passed"

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

--     let r = part_one input
--     print r
--     print $ assert (0 == r) "part one passed!"

--     let r = part_two input
--     print r
--     print $ assert (0 == r) "part two passed!"
