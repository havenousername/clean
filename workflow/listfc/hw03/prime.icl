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
primeFactors n = factor n 

factor :: Int  -> [Int]
factor x = [n \\ n <-[2..x] | (x rem n == 0) && (isPrime n)]

// Start = primeFactors 614889782588491410 // [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]
Start = primeFactors 24// [2, 3]


// Start = primeFactors 200



