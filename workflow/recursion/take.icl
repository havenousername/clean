module take
import StdEnv

fib :: Int -> Int
fib n 
| n == 0 = 0
| n == 1 = 1
= fib(n-1)+fib(n-2)

//fibInf :: Int -> [Int]
//fibInf x = [fib(x)] ++ fibInf(x + 1)

//Start = fibInf 0
//Start = [1..]

fib2 :: Int -> Int
fib2 n = fibAux n 0 1

fibAux :: Int Int Int -> Int
fibAux 0 _ b = b
fibAux n a b = fibAux (n-1) b (a+b)

Start = take 50 [fib2 n \\ n<-[1..]]