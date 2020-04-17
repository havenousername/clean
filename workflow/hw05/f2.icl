module f2
import StdEnv

// f3

// Splits the list into sublist. 
splitAgain::Int [Int] -> [[Int]]
splitAgain _ [] = []
splitAgain y xs = [take (y) xs] ++ splitAgain (y) (tl xs)

// Collects all splited subsequences
splitSeq:: Int [Int] -> [[Int]]
splitSeq 0 _ = []
splitSeq y x = splitAgain y x ++ splitSeq (y-1) x 

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


Start = f3 [1,1,2,4,5,3,2,6,3] // [4,5,3,2,6]
// Start = f3 [] // []
// Start = f3 [3] // [3]
// Start = f3 [1,1,2,4,5,3,2,6,3,1,2,3,2,4,5,4,6,8,9,12,3,4,5,56,6,7,1,2,3,4,5] // [8,9,12,3,4,5,56,6,7,1,2]


