import qualified Data.List as L
import qualified Data.List.Split as S

type Box = (Int, Int, Int)

parse_box :: String -> Box
parse_box s =
    let [a, b, c] = L.sort $ map read (S.splitOn "x" s)
    in (a, b, c)

to_sa :: Box -> Box
to_sa (l, w, h) = (l*w, l*h, w*h)

wrap :: Box -> Int
wrap b =
    let (s, m, l) = to_sa b
    in s * 3 + m * 2 + l * 2

wrap_order :: [Box] -> Int
wrap_order bs = sum (map wrap bs)

ribbon :: Box -> Int
ribbon (a, b, c) = a * 2 + b * 2 + a * b * c

ribbon_order :: [Box] -> Int
ribbon_order bs = sum (map ribbon bs)

main = do
    input <- readFile "day02_input.txt"
    print (wrap_order (map parse_box (lines input)))
    print (ribbon_order (map parse_box (lines input)))
