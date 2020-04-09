module reverse
import StdEnv


//Start = foldr operation initialElement [list of elements]

// Define function "reverse" using foldr
f3:: [Int] -> [Int]
f3 x = foldr (\x xs = xs ++ [x]) [] x

// Start = f3 [1..4]
// Start = f3 [1,2,3,4,5,6,7,8] // [8,7,6,5,4,3,2,1]
// Start = f3 [] // []
Start = f3 [1] // [1]
