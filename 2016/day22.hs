import Utils

data Point = Point Int Int
           deriving (Eq, Show)

data Node = Node Point Int Int
          deriving (Eq, Show)

parse :: String -> Node
parse s =
    let ps = regexgrps s "/dev/grid/node-x([0-9]+)-y([0-9]+) +([0-9]+)T +([0-9]+)T +([0-9]+)T +([0-9]+)%"
        ns = map read ps
    in Node (Point (ns!!0) (ns!!1)) (ns!!3) (ns!!4)

parse_file :: String -> [Node]
parse_file = map parse . lines

part_one :: String -> Int
part_one input = length input

--part_two :: String -> Int
--part_two input = length input

test_input = ""

main = do
    -- Filesystem              Size  Used  Avail  Use%
    input <- readFile "day22_input.txt"

    assert_equal (Node (Point 0 8) 66 20) (parse "/dev/grid/node-x0-y8     86T   66T    20T   76%") "parse"

    assert_equal [ (Node (Point 0 20) 67 20)
                 , (Node (Point 0 21) 71 15)
                 , (Node (Point 4 0) 70 17)
                 ] (parse_file "/dev/grid/node-x0-y20    87T   67T    20T   77%\n\
                               \/dev/grid/node-x0-y21    86T   71T    15T   82%\n\
                               \/dev/grid/node-x4-y0     87T   70T    17T   80%") "parse_file"

--     assert_equal 0 (part_one input) "part one"

--     assert_equal 0 (part_two input) "part two"
