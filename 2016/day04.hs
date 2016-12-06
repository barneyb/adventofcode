import Control.Exception (assert)
import Text.Regex.TDFA

data Room = Room {
    name   :: String,
    sector :: Int,
    chksum :: String
} deriving (Eq, Show)

room_regex :: String
room_regex = "([a-z-]+)-([0-9]+)\\[([a-z]{5})\\]"

parse_room :: String -> Room
parse_room s =
    let (_, _, _, ps) = s =~ room_regex :: (String,String,String,[String])
    in  Room{name=ps !! 0, sector=read (ps !! 1), chksum=ps !! 2}

part_one :: String -> Int
part_one input = length input

test_rooms = [
             "aaaaa-bbb-z-y-x-123[abxyz]",
             "a-b-c-d-e-f-g-h-987[abcde]",
             "not-a-real-room-404[oarel]",
             "totally-real-room-200[decoy]"
             ]

main = do
    input <- readFile "day04_input.txt"
    foldl1 (>>) (map (\s -> print (parse_room s)) test_rooms)
--    print (part_one input)
--    print $ assert (1032 == (part_one input)) "part one passed!"
