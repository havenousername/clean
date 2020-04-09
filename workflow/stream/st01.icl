module st01
import StdEnv

// Starting with recursion

// Recursion is reapeating something.
// Recursion has two parts
// 1. Reapeated action
//    a. What are we doing over and over again 
//    b. How does it repeat?
// 2. What is the stop conditions?
//    a. When do we stop a recursion?
//    b. How do we stop a recursion?
//    c. What do we do after we stop a recursion?

// Lets do factorial of n 
// What should we get
// factorial 4 = 4*3*2*1

/*
Side recursion
1. Reapeated action
   a. What are we doing over and over again ? -> multiplication
   b. How does it repeat? -> multiply by the next number down
2. What is the stop conditions? -> when we hit one 
   a. When do we stop a recursion?
   b. How do we stop a recursion?
   c. What do we do after we stop a recursion?
*/

// code the basic step first 
factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n 
| n < 0 = 0
= n*factorial(n-1)
// Start = factorial 4

// Nested recursion
// We want to square number repeatedly
// (((2)^2)^2)^2)

/*
1. Reapeated action
   a. What are we doing over and over again ? -> squareing
   b. How does it repeat? -> squaring inside parentesis
2. What is the stop conditions? -> when we have squared enough
   a. When do we stop a recursion?
   b. How do we stop a recursion? -> return that number itself
   c. What do we do after we stop a recursion?
*/

squaringBack :: Int Int -> Int
squaringBack num 0 = num
squaringBack num howMany = (squaringBack num (howMany-1))^2

// Start = squaringBack 2 3

// Recursion with list

// repeatn 

// Repeated Action : putting elements into a list
// Stop condition : when we put enough elements inside the list
repeat :: Int a -> [a]
repeat 0 _ = [] 
repeat counter insert 
| counter < 0 = []
= [insert] ++ repeat(counter-1) insert

repeatN :: Int a -> [a]
repeatN 0 _ = [] 
repeatN counter insert 
| counter < 0 = []
= [insert:repeatN(counter-1) insert]



// Start = repeat 1000000000 "Hello"

// Tail recursion - is recursion that is linear on time
// It takes all advantages of tail recursion optimazation which is done by almost all modern compilers 
// It requires an additional variable, called accumulator 

// We Must compete evaluation on each step
repeatNTail :: Int a -> [a]
repeatNTail howMany elem  
| howMany < 0 = []
=  repeatNTailAux howMany elem []

repeatNTailAux :: Int a [a] -> [a]
repeatNTailAux 0 _ accum = accum
repeatNTailAux howMany elem accum = repeatNTailAux (howMany-1) elem (accum++[elem])

// Start = repeatNTail 10000000 "Hi"

addUp :: Int Int -> Int
addUp a b 
| a > b = addUp b a 
= addUpAux a b [] 

addUpAux :: Int Int [Int] -> Int
addUpAux a b accum 
| a == b = sum accum
= addUpAux (a+1) b (accum ++ [a])

// Start = addUp 5 10

/*
  Given the list of sublist numbers
  Leave only sublist all prime numbers in the list
*/

// Start = [[1,2,3],[4,9,10 ]]

condEven :: [Int] -> Bool
condEven ourList = and(map isEven ourList)

// Start = condEven [2,4,4,6]

// Start = filter condEven [[2,3,4,5],[2,4,6],[1..5]]

primeList :: [Int] -> Bool 
primeList ourList = and(map isPrime ourList)

isPrime :: Int -> Bool
isPrime n = not(or(map (dividable n) [2..(n-1)]))

dividable :: Int Int -> Bool 
dividable n check = n rem check == 0

primeListFilter :: [[Int]] -> [[Int]]
primeListFilter ourList =  filter primeList (filter notEmpty ourList)

notEmpty :: [Int] -> Bool
notEmpty list = not(isEmpty list)

Start = primeListFilter [[1,2],[4,6,8],[]]
