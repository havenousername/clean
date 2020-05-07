module stream 
import StdEnv



isPrime x 
| x <= 1 = False
= isEmpty[n \\ n <- [2..(x-1)]| x rem n == 0]

primeList :: [Int]
primeList = [x \\ x <- [1..] | isPrime x]


Start = takeWhile ((<=) 10) primeList