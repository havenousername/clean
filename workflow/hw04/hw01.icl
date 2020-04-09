module hw01
import StdEnv

// Write function that returns length of a list
// You must use foldr or foldl

f2 :: [Int] -> Int
f2 x = foldr (+) 0 (map (\x = 1) x)


// Start = lengthOflist [1..3]
// Start = f2 [] // 0
Start = f2 (take 100 [1..]) // 100

// Start = f2 [1,2,3] // 3
// Start = f2 [1] // 1
