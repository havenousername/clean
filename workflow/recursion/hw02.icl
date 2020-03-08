module hw02
import StdEnv

// Fibonacci

fibo :: Int -> [Int]
fibo n
| n < 0 = []
= fiboX n 0 1

fiboX :: Int Int Int -> [Int]
fiboX n a b
| a > n = []
= [a] ++ fiboX n b (b+a)

// Start = fibo 1

// Prime number

isPrime :: Int -> Bool 
isPrime n 
| n < 1 = False
= prime n (n-1)

prime :: Int Int -> Bool
prime n m
| m == 1 = True
| n rem m == 0 = False
= prime n (m-1)

// Start = isPrime 19

// Start = (15/2)*2

// Parindrone

sequence :: Int -> [Int]
sequence n
| n == 0 = []
= sequence(n/10) ++ [n rem 10]

isPalindrone :: Int -> Bool 
isPalindrone n 
| n < 0 = False
= reverse(sequence n) == sequence n

Start = isPalindrone 733337



