module hw_04
import StdEnv

// For every sublist, eliminates its elements
// Until the current element is a prime number
// After that, multiply each number by 5
// And remove all elements that end with 0. I.e. divisible by 10.

//  task 01 

divisible :: Int Int -> Bool
divisible x n = x rem n == 0

denominators :: Int -> [Int]
denominators x = filter (divisible x) [1..x]

prime :: Int -> Bool
prime x 
| x == 1 = False
= denominators x == [1,x]

notPrime :: Int -> Bool
notPrime x = not(prime x)


notDivisibleByTen :: Int -> Bool
notDivisibleByTen x = not(x rem 10 == 0)

multiplyFive :: [Int] -> [Int]
multiplyFive [] = []
multiplyFive [x:xs] = [x*5 : multiplyFive xs] 

dropWhileCustom :: (a->Bool) [a] -> [a]
dropWhileCustom p [] = []
dropWhileCustom p [x:xs]
| p x = dropWhileCustom p xs
= [x:xs]
 
f1 :: [[Int]] -> [[Int]]
f1 [] = []
f1 [x:xs] = [filter notDivisibleByTen (multiplyFive (dropWhileCustom notPrime x)) : f1 xs] 



// Start = filter prime [1, 2, 8]
// Start =  f1 [[1,2,3]]

// Start = f1 [[1, 2, 3, 4], [9, 7, 6, 5, 4, 3, 0], [3, 5, 7, 9], [], [128, 64, 32]] // [[15],[35,25,15],[15,25,35,45],[],[]]
// Start = f1 [[1], [4], [2]] // [[],[],[]]
// Start = f1 [[5..10], map (\x = x + 5) [1..4], [], [4,12,8,5, 4]] // [[25,35,45],[35,45],[],[25]]


// Start = multiplyFive [1..10]


// task 02


// Write function that returns length of a list
// You must use foldr or foldl

f2 :: [Int] -> Int
f2 x = foldr (+) 0 (map (\x = 1) x)


// Start = f2 [1..3]
// Start = f2 [] // 0
// Start = f2 (take 100 [1..]) // 100

// Start = f2 [1,2,3] // 3
// Start = f2 [1] // 1


// task 3

//Start = foldr operation initialElement [list of elements]

// Define function "reverse" using foldr
mySort:: [Int] -> [Int]
mySort [] = []
mySort [x:xs] = mySort [y \\ y <- xs | y > x] ++ [x] ++ mySort [y \\ y <- xs | y < x]


Start = mySort [2,3,2,1,4,3,3,5]

// Start = f3 [1..4]
// Start = f3 [1,2,3,4,5,6,7,8] // [8,7,6,5,4,3,2,1]
// Start = f3 [] // []
// Start = f3 [1] // [1]





