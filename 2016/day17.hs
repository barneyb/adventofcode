import Control.Exception (assert)
import qualified Data.List as L
import Debug.Trace
import Utils

data Pos = Pos Int Int String
         deriving (Eq, Show)

derive :: (String -> String) -> Pos -> [Pos]
derive hf (Pos x y path) =
    let doors = map (`elem` "bcdef") (hf path)
    in map snd $ filter
        (\(open, (Pos x' y' _)) -> open && x' `elem` [1..4] && y' `elem` [1..4])
        (zip doors [ Pos x     (y-1) (path++"U")
                   , Pos x     (y+1) (path++"D")
                   , Pos (x-1) y     (path++"L")
                   , Pos (x+1) y     (path++"R")
                   ])

walk :: (String -> String) -> (Int -> Int -> Bool) -> Pos -> Maybe Pos
walk hf plt p = go p
    where
        go :: Pos -> Maybe Pos
        go p@(Pos 4 4 path) = Just p
        go p@(Pos x y path) =
            let nexts = map go (derive hf p)
                nexts' = filter f nexts
            in if length nexts' == 0
                then Nothing
                else head $ L.sortBy (\(Just (Pos _ _ p)) (Just (Pos _ _ p')) ->
                    let l = length p
                        l' = length p'
                    in if l == l' then EQ else if l `plt` l' then LT else GT) nexts'

        f :: Maybe Pos -> Bool
        f Nothing = False
        f (Just p) = True

part_one :: String -> String
part_one input =
    let Just (Pos _ _ path) = walk (\s -> md5 (input ++ s)) (<) (Pos 1 1 "")
    in path

part_two :: String -> Int
part_two input =
    let Just (Pos _ _ path) = walk (\s -> md5 (input ++ s)) (>) (Pos 1 1 "")
    in length path

equals :: (Eq a, Show a) => a -> a -> String -> IO ()
equals expected actual message = putStrLn ("[Test: " ++ message ++ "]") >>
    if expected == actual
        then putStrLn "PASS!"
        else prints [ "expected : " ++ (show expected)
                       , "actual   : " ++ (show actual)
                       , "FAILURE"
                       ]

main = do
    let input = "veumntbg"

    equals "DDRRRD" (part_one "ihgpwlah") "example one"
    equals "DDUDRLRRUDRD" (part_one "kglvqrro") "example two"
    equals "DRURDRUDDLLDLUURRDULRLDUUDDDRR" (part_one "ulqzkmiv") "example three"

    equals "DDRRULRDRD" (part_one input) "part one"

    equals 536 (part_two input) "part two"
