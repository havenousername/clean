module f2
import StdEnv


// find the sum of all odd squares that are smaller than 10,000
//f1 :: Int
f1:: Int
f1 = foldr (+) 0 [ x^2 \\ x <- [1..10000]| isOdd x && x^2 < 10000]

// Start = f1

// f2
// Splits the list into sublist. 
splitAgain::Int [Int] -> [[Int]]
splitAgain _ [] = []
splitAgain y xs = [take (y) xs] ++ splitAgain (y) (tl xs)

// Collects all splited subsequences
splitSeq:: Int [Int] -> [[Int]]
splitSeq 0 _ = []
splitSeq y x = splitAgain y x ++ splitSeq (y-1) x 

// Filters the split 
splitFinal:: [Int] -> [[Int]]
splitFinal x =[subseq \\ subseq <- removeDup (splitSeq (length x) x)]

// just length
length:: [Int] -> Int
length x = foldr (+) 0 (map (\x = 1) x)

// Converts to the normal list 
addSeq:: [[Int]] -> [Int]
addSeq []  = []
addSeq [x:xs] = [addAllSeq x] ++ addSeq xs

// Adds element inside sequence
addAllSeq:: [Int] -> Int
addAllSeq x = foldr (+) 0 x

// Final fucntion
f2:: [Int] -> Int
f2 x = length (removeDup (addSeq(splitFinal x)))


// Start = f2 [1,2,3,4] // 9
// Start = f2 [] // 0
// Start = f2 [3] // 1
// Start = f2 [1,-3,2,-4,-3,1,7,6,2,8,9] // 34
// Start = f2 [1,1,2,4,5,3,2,6,3,1,2,3,2,4,5,4,6,8,9,12,3,4,5,56,6,7,1,2,3,4,5] // 166


// f3
isEqual:: [a] [a] -> Bool | == a
isEqual [] [] = True
isEqual [] xs = False
isEqual ys [] = False
isEqual [x:xs] [y:ys] 
| x <> y = False
| x == y = isEqual xs ys

notIsEqual:: [a] [a] -> Bool | == a
notIsEqual x u = not(isEqual x u)


quickDelete:: [[Int]] -> [[Int]] 
quickDelete [] = []
quickDelete all=:[first:rest] =  quickDelete[x \\ x <- rest | notIsEqual first x] ++ [first]

quickSort:: [[Int]] -> [[Int]] 
quickSort [] = []
quickSort all=:[first:rest] =  quickSort[x \\ x <- rest | length (x) <= length (first)] ++ [first] ++ quickSort[x \\ x <- rest | length (x) > length (first)]

splitNew:: [Int] -> [[Int]]
splitNew x = quickDelete ([(subseq) \\ subseq <- splitSeq (length x) x | seqNoDup subseq])

takeBiggest:: [[Int]] -> [Int]
takeBiggest [x] = x
takeBiggest [x:xs]
| (length x) > (length (hd xs)) = x
= takeBiggest xs

seqNoDup:: [Int] -> Bool
seqNoDup [] = True
seqNoDup [x:xs] 
| isMember x xs = False
= seqNoDup xs


NoDuplicates:: [[Int]] -> [[Int]]
NoDuplicates listL = [x \\ x <- listL ]

f3 :: [Int] -> [Int]
f3 [] = []
f3 x = takeBiggest (quickSort (splitNew x))


// Start = f3 [1,1,2,4,5,3,2,6,3] // [4,5,3,2,6]
// Start = f3 [] // []
// Start = f3 [3] // [3]
Start = f3 [1,1,2,4,5,3,2,6,3,1,2,3,2,4,5,4,6,8,9,12,3,4,5,56,6,7,1,2,3,4,5] // [8,9,12,3,4,5,56,6,7,1,2]


