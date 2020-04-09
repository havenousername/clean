module lol
import StdEnv

foo:: Int -> bool
foo x = x rem 2 == 1

bar:: Int -> Int
bar x = 2*x + x/2

Start = filter foo (map bar [1..6])