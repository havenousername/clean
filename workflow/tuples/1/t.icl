module t
import StdEnv

// Start = map ((+)1) [1..10]

// Start = (+) 2 3

// Start = foldr (+) 0 [1..3] // ((+) 3 0)

// (-) arg1(list) agr2(0) -> (-) last from the list / inicial

// map filter takeWhile dropWhile [\\]

// Start = foldr (\ newEl list =  [newEl *2] ++list  ) [] [1..10]

// Start = map ((*)2) [1..10]

//  folder function neutralElement [ListofElToProcess]

// Start = foldr (*) 1 [1..5]

// Start = foldr (\newEl list | isEven newEl = [newEl^3] ++ list = list) [] [1..10] 



// same
// Start = [n^3  \\ n <- [1..10] | isEven n]


/*

given  a list a numners. 
*/

// prime numbers
isPrime:: Int -> Bool
isPrime x = and[ x rem n <> 0 \\ n <- [2..(x-1)]]

// ^2
listSq :: [Int] -> [Int]
listSq list = map (\x = x^2) list
// less than n 

lessThan :: [Int] Int -> [Int]
lessThan list n =  filter (\x = x < n) list




finalFunc:: [Int] Int  -> [Int]
finalFunc list x  = listSq (lessThan (filter isPrime list) x) 

decimalPart :: Real -> Real
decimalPart x = result
where
  intVer = toInt x
  result = x - toReal(intVer)




Start = finalFunc [1..10] 9
