module prime
import StdEnv

isPrime :: Int -> Bool 
isPrime n = prime n (n-1)

prime :: Int Int -> Bool
prime n m
| m == 1 = True
| n rem m == 0 = False
=prime n (m-1)

// Start = prime 

primeFactors :: Int -> [Int]
primeFactors n = factor n 2 

factor :: Int Int -> [Int]
factor x y
| y < 2  = []
= [n \\ n <-[y..x] | (x rem n == 0) && (isPrime y)]
// | (x rem y == 0) && (isPrime y) = [y] ++ factor x (y-1)






Start = primeFactors 12
// Start = primeFactors 614889782588491410 // [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
// Start = isPrime 614889782588491410


// fib3 :: Int -> Int
// fib3 n = fibAux n 1 1 

// fibAux 0 a b = a
// fibAux n a b | n > 0 = fibAux (n-1) b (a+b)

// fibAux 5 1 1 | 5 > 0 = fibAux 4 1 2
// fibAux 4 1 2 | 4 > 0 = fibAux 3 1 3
// fibAux 3 1 3 |  > 0 = fibAux 3 1 3

// Start = primeFactors 200



