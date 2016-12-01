import Control.Exception (assert)
import qualified Data.Map.Strict as Map

to_ins_list :: String -> [[String]]
to_ins_list s = map words (lines s)

type Circuit = Map.Map String (Either [String] Int)

wire_circuit :: [[String]] -> Circuit
wire_circuit ins = foldl (\c w -> Map.insert (last w) (Left (init (init w))) c) Map.empty ins

evaluate :: Circuit -> String -> Int
evaluate c x
    | (head x) `elem` ['0'..'9']    = read x
    | otherwise                     = emulate c x

emulate :: Circuit -> String -> Circuit
emulate c w = Map.insert w (Right (val v)) c
    where
        Just v = Map.lookup w c
        val :: Either [String] Int -> Int
        val (Left ps) = evaluate c ps
        val (Right n) = n

part_one :: String -> String -> Int
part_one input wire =
    let
        Just (Left v) = Map.lookup wire (wire_circuit (to_ins_list input))
    in length v

main = do
    input <- readFile "day06_input.txt"
    print (part_one "123 -> x" "x")
    print $ assert (123 == part_one "123 -> x" "x") "signal worked"
--    456 -> y
--    x AND y -> d
--    x OR y -> e
--    x LSHIFT 2 -> f
--    y RSHIFT 2 -> g
--    NOT x -> h
--    NOT y -> i
    -- 3176
    -- 14710
