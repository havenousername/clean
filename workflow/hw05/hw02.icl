module hw02
import StdEnv

// Given list of integers, find number of
// different sums of continuous subsequences
// Example: [1,2,3,4] has 10 continuous subsequences
// [1], [2], [1,2], [3], [2,3], [1,2,3], [4], [3,4], [2,3,4], [1,2,3,4]
// And there sums are [1,2,3,3,5,6,4,7,9,10], from which we need to remove
// duplicates and we get [1,2,3,4,5,6,7,9,10] - Hence we ave 9 different sums
// https://stackoverflow.com/questions/3988575/what-does-this-definition-of-contiguous-subsequences-mean


// subS :: [Int] -> [[Int]]
split:: Int [Int] -> [[Int]]
split n xs = [take n xs] ++ [drop n xs]

splitAgain:: [Int] -> [[Int]]
splitAgain xs = [take 1 xs] ++ splitAgain (tl xs)

length:: [Int] -> Int
length x = foldr (+) 0 (map (\x = 1) x)


splitOne:: [Int] Int -> [[Int]]
splitOne x 0 = []
splitOne x y = split y x ++ splitOne (x) (y-1)

finalSplit:: [Int] -> [[Int]]
finalSplit x  = splitAgain x ++ splitOne x (length x) 


remove:: Int [Int] -> [Int]
remove x [] = []
remove x [y:ys] 
| x == y = remove x ys
| otherwise = [y:remove x ys]  

removeDublicates:: [a] -> [a]
removeDublicates [] = []
removeDublicates [x:xs] = [x : removeDublicates(remove x xs)]

isEqual:: [Int] [Int] -> Bool
isEqual [] [] = True
isEqual [] xs = False
isEqual ys [] = False
isEqual [x:xs] [y:ys] 
| x <> y = False
| x == y = isEqual xs ys


// removeL :: [[Int]] -> [[Int]]
// removeL bigSublist = remove Dup finalSplit


Start = splitOne  [1..4]
// Start = removeL [[1..2],[1..2]]
// Start = removeDup [ [1..4], [1..4], [1..3],[] ]




// splitOne:: [Int] Int -> [[Int]]
// splitOne x 0 = []
// splitOne x y = split y x ++ splitOne x (y-1)

// finalSplit:: [Int] -> [[Int]]
// finalSplit x  = [subsplit \\ subsplit <- splitAgain 1 x ++ splitOne x (length x) ++ splitAgain1 x | not(isEmpty subsplit)]


// splitSeq:: Int [Int] -> [[Int]]
// splitSeq 0 _ = []
// splitSeq y x = splitAgain y x ++ splitSeq (y-1) x 