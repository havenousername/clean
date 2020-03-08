module tail 
import StdEnv

take :: Int [a] -> [a]
take n [] = []
take n [x: xs]
| n < 1 = [] 
|otherwise = [x : take(n-1) xs]

drop :: Int [a] -> [a]
drop n [] = []
drop n [x:xs]
| n < 1 = [x:xs]
| otherwise = drop(n-1) xs 

reverse :: [a] -> [a]
reverse [] = []
reverse [x:xs] = reverse xs ++ [x]

sum :: [Int] -> Int
sum [] = 0
sum [x:xs] = x + sum xs

sum2 :: [Int] -> Int
sum2 x
| x == [] = 0
=hd x + sum2(tl x)

Start = sum2 [1..3]

