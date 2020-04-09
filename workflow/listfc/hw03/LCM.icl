module LCM
import StdEnv


listmultiples :: Int Int -> [Int]
listmultiples 1 y = [] 
listmultiples x y
| x rem y == 0 = [y : listmultiples (x/y) y]
= listmultiples x (y+1)

listmultiple :: Int -> [Int]
listmultiple x = listmultiples x 2


gCD :: Int Int -> Int
gCD x y 
| y == 0 = x
= gCD y  (x rem y) 

lcm :: [Int] -> Int
lcm [] = 0
lcm [x] = x
lcm [x,y] = x*y / gCD x y
lcm [x:xs] = lcm [x,lcm xs]

Start  = lcm [1, 10, 400453, 58359, 5389538]

