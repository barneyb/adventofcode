import qualified Data.Map.Strict as Map

doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

doubleSmallNumber x = if x > 100
    then x
    else doubleMe x

boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

length' :: [a] -> Int
length' xs = sum [ 1 | _ <- xs ]

length'' :: [a] -> Int
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

triangles = [ (a,b,c) | a <- [1..10], b <- [a..10], c <- [b..10], a^2 + b^2 == c^2, a+b+c == 24 ]

removeNonUppercase :: String -> String
removeNonUppercase s = [ c | c <- s, c `elem` ['A'..'Z'] ]

--addThree :: Int -> Int -> Int -> Int
addThree :: Num a => a -> a -> a -> a
addThree x y z = x + y + z

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x

tail' :: [a] -> [a]
tail' [] = error "you can't tail an empty list, silly!"
tail' (_:xs) = xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of '" ++ all ++ "' is '" ++ [x] ++ "'"

max' :: (Ord a) => a -> a -> a
max' x y
    | x >= y    = x
    | otherwise = y

compare' :: (Ord a) => a -> a -> Ordering
compare' a b
    | a < b     = LT
    | a > b     = GT
    | otherwise = EQ

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where
        bmi = weight / height ^ 2
        skinny = 18.5
        normal = 25.0
        fat = 30.0

initials :: String -> String -> String
initials (f:_) (l:_) = [f, l]

initials' :: String -> String -> String
initials' first last = [f, l]
    where
        (f:_) = first
        (l:_) = last

calcBmis :: RealFloat a => [(a, a)] -> [a]
-- comprehension inline
--calcBmis ps = [ w / h ^ 2 | (w, h) <- ps ]
-- comprehension w/ let
calcBmis ps = [ bmi | (w, h) <- ps, let bmi = w / h ^ 2 ]
-- comprehension w/ where
--calcBmis ps = [ bmi p | p <- ps]
-- recursion w/ where
--calcBmis [] = []
--calcBmis (p:ps) = bmi p :calcBmis(ps)
--    where
--        bmi (w, h) = w / h ^ 2

maximum' :: Ord a => [a] -> a
maximum' [] = error "empty list doesn't have a max"
maximum' [x] = x
--maximum' (x:xs)
--    | x > tm    = x
--    | otherwise = tm
--    where tm = maximum' xs
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
--replicate' 0 _ = []
--replicate' n x = x:replicate' (n - 1) x
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ [] = []
take' n (x:xs) = x:take' (n - 1) xs

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' e (x:xs)
    | e == x    = True
    | otherwise = elem' e xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let small = quicksort (filter (<=x) xs)
        large = quicksort (filter (>x) xs)
    in small ++ x:large

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f x y = f y x

-- find the largest number under 100,000 that's divisible by 3829
largestDivisible :: Integral a => a
largestDivisible = head (filter test [100000,99999..0])
    where test n = n `mod` 3829 == 0

-- for all starting numbers between 1 and 100, how many chains have a length greater than 15
collatz :: Integral a => a-> [a]
collatz 1 = [1]
collatz n
    | even n    = n : collatz (n `div` 2)
    | odd n     = n : collatz (n * 3 + 1)

numLongChains :: Int
--numLongChains = length (filter (>15) (map length (map collatz [1..100])))
numLongChains = length (filter (\xs -> length xs > 15) (map collatz [1..100]))

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

oddSquareSum :: Integer
--oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
--oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n - 1) + fib (n - 2)

mfib :: Int -> Int
mfib = (map fib [0..] !!)
    where
        fib 1 = 1
        fib 2 = 1
        fib n = mfib (n - 1) + mfib (n - 2)
