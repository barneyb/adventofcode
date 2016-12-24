import Control.Exception (assert)
import qualified Data.List as L
import qualified Data.Char as C
import Data.Array
import Utils

data Register = A
              | B
              | C
              | D
              deriving (Eq, Ord, Enum, Bounded, Show, Ix)

data Instruction = Copy Register Register
                 | Load Int Register
                 | Inc Register
                 | Dec Register
                 | Jump Int
                 | JumpNZ Register Int
                 deriving (Eq, Show)

type Prog = Array Int Instruction

type Proc = Array Register Int

is_number :: String -> Bool
is_number s = C.isDigit (last s)

to_register :: String -> Register
to_register "a" = A
to_register "b" = B
to_register "c" = C
to_register "d" = D

parse :: String -> [Instruction]
parse input = map p (lines input)
    where
        p :: String -> Instruction
        p s
            | "cpy" `L.isPrefixOf` s =
                let ps = regexgrps s "cpy ([0-9]+|[a-d]) ([a-d])"
                in if (is_number (ps!!0)) then
                    Load (read (ps!!0)) (to_register (ps!!1))
                else
                    Copy (to_register (ps!!0)) (to_register (ps!!1))
            | "inc" `L.isPrefixOf` s =
                let r = regexgrp s "inc ([a-d])"
                in Inc (to_register r)
            | "dec" `L.isPrefixOf` s =
                let r = regexgrp s "dec ([a-d])"
                in Dec (to_register r)
            | "jnz" `L.isPrefixOf` s =
                let ps = regexgrps s "jnz ([0-9]+|[a-d]) (-?[0-9]+)"
                    d = read (ps!!1)
                in if (is_number (ps!!0)) then
                    let n = read (ps!!0)
                    in Jump (if n == 0 then 1 else d)
                else
                    JumpNZ (to_register (ps!!0)) d

to_prog :: [Instruction] -> Prog
to_prog is = array (1, length is) (zip [1..] is)

execute :: Proc -> Prog -> Proc
execute proc prog = proc

part_one :: String -> Int
part_one input =
    let proc = array (A, D) [(r, 0) | r <- range(minBound, maxBound)]
        proc' = execute proc (to_prog (parse input))
    in proc!A

--part_two :: String -> Int
--part_two input = length input

test_input = "cpy 41 a\n\
             \inc a\n\
             \inc a\n\
             \dec a\n\
             \jnz a 20\n\
             \jnz 1 7\n\
             \jnz 0 1\n\
             \cpy a b\n\
             \dec a\n"

test_instructions = [ Load 41 A
                    , Inc A
                    , Inc A
                    , Dec A
                    , JumpNZ A 20
                    , Jump 7
                    , Jump 1 -- a no-op
                    , Copy A B
                    , Dec A
                    ]

main = do
    input <- readFile "day12_input.txt"

    let r = parse test_input
    print ("expected : " ++ show test_instructions)
    print ("actual   : " ++ show r)
    print $ assert (test_instructions == r) "parse passed"

    let r = part_one test_input
    print r
    print $ assert (42 == r) "example one passed!"

--     let r = part_one input
--     print r
--     print $ assert (0 == r) "part one passed!"

--     let r = part_two input
--     print r
--     print $ assert (0 == r) "part two passed!"
