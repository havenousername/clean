module high
import StdEnv

something :: Int Int -> Int
something a b = a ^ b


f :: Int Int (Int Int -> Int) -> Int
f a b op = op a b 

// Start = f 2 3 (+) // (+) 2 3 = 2 + 3
// Start = f 2 3 something // something 2 3 

// isEven :: Int -> Bool
// [1..10]

// Filter

// Start = filter isEven [1..10] // [2,4,6,8]

// Start = filter ((>) 5) [1..10]
// Start = filter (\x = x < 5) [1..10]  //Lambda expression = inline functions

// (\x = x<5) -> (>) 5 1  -> 5 > 1
// ((>)5) 1 -> (>) 5 1 -> 5 > 1 
isPrime :: Int -> Bool
isPrime x = and[x rem n <> 0 \\ n <-[2..(x-1)]]


// Start = filter (\x = x > 50) (filter isPrime [1..100])
// Start = filter (\x = isPrime x && x > 50) [1..100]

// map!!!

// Start = map((+) 2) [1..10]

// Start = map isEven [1..10]
// Start = map (\x = x+5 > 3) [1..10] // [True,True,True,True,True,True,True,True,True,True]
 

//  Can we use map with isMember? yes, but be careful

// isMember :: a [a] -> Bool

isInList :: [Int] [Int] -> [Int]
isInList list1 list2 = filter (\somenum  = isMember somenum list1) list2


// Start = isInList [2,4,5,3,6,4,6,1,4] [3..9] //3,4,5,6]

// curring


// Start = foldr operation inicialEl [list of el]
// Start = foldr (+) 100 [1..3]

// Start = foldr (-) 5 [1..3]

// Start = foldl (-) 5 [1..3]

//  (-) 5 1 = 4
//  (-) 4 2 = 2
//  (-) 4 3 = -1

// Start = foldr (\x y = [x]++y) [] [1..10]

bigOdds :: [Int] -> [Int]
bigOdds list  = filter (\some = isOdd some && some > 10) list

// Start = bigOdds [1..20]
// Start = bigOdds [1..20]//[11,13,15,17,19]
// Start = bigOdds [1..100]
// Start = bigOdds []
// Start=bigOdds [1..10]//[]
Start = bigOdds [12,14..100]//[]


