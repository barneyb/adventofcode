import Control.Exception (assert)
import Text.Regex.TDFA
import qualified Data.Map.Strict as M
import qualified Data.List as L
import Utils

data Room = Room {
    encname  :: String,
    sector   :: Int,
    checksum :: String
} deriving (Eq, Show)

room_regex :: String
room_regex = "([a-z-]+)-([0-9]+)\\[([a-z]{5})\\]"

parse_room :: String -> Room
parse_room s =
    let (_, _, _, ps) = s =~ room_regex :: (String,String,String,[String])
    in  Room{encname=ps !! 0, sector=read (ps !! 1), checksum=ps !! 2}

parse_rooms :: String -> [Room]
parse_rooms input = map parse_room (lines input)

chksum :: String -> String
chksum s =
    let h = M.toList $ hist $ filter (/= '-') s
    in take 5 $ map fst  $L.sortBy (\b a -> compare (snd a) (snd b)) h

is_real :: Room -> Bool
is_real r = checksum r == (chksum $ encname r)

part_one :: String -> Int
part_one input = foldl (\s r -> s + (sector r)) 0 (filter is_real (parse_rooms input))

decrypt :: Int -> Char -> Char
decrypt s '-' = ' '
decrypt s c = head $ drop (mod s 26) ([c..'z'] ++ ['a'..(pred c)])

name :: Room -> String
name r = map (decrypt (sector r)) (encname r)

part_two :: String -> Int
part_two input = sector $ head (filter (\r -> (is_real r) && (name r) == "northpole object storage") (parse_rooms input))

test_input = "aaaaa-bbb-z-y-x-123[abxyz]\n\
             \a-b-c-d-e-f-g-h-987[abcde]\n\
             \not-a-real-room-404[oarel]\n\
             \totally-real-room-200[decoy]"

main = do
    input <- readFile "day04_input.txt"
    foldl1 (>>) (map (\s -> print (parse_room s)) (lines test_input))
    foldl1 (>>) (map (\s -> print (is_real $ parse_room s)) (lines test_input))
    print $ assert (1514 == (part_one test_input)) "test one passed!"
    print $ assert (361724 == (part_one input)) "part one passed!"
    print $ assert ("very encrypted name" == (name Room{encname="qzmt-zixmtkozy-ivhz", sector=343, checksum=""})) "test two passed!"
    print $ assert (482 == part_two input) "part two passed!"

