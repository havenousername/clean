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
repeat :: Int a -> [a]
repeat 0 insert = [] 
repeat counter insert = [insert:repeat(counter-1) insert]

Start = repeat 10 "Hello"