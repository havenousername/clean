module hw01
import StdEnv

// find the sum of all odd squares that are smaller than 10,000
//f1 :: Int
f1:: Int
f1 = foldr (+) 0 [ x^2 \\ x <- [1..10000]| isOdd x && x^2 < 10000]

Start = f1