module h
import StdEnv


split:: Int [Int] -> [[Int]]
split n xs = [take n xs] ++ [drop n xs]

splitAgain::Int [Int] -> [[Int]]
splitAgain _ [] = []
splitAgain y xs = [take (y) xs] ++ splitAgain (y) (tl xs)


length:: [Int] -> Int
length x = foldr (+) 0 (map (\x = 1) x)


splitSeq:: Int [Int] -> [[Int]]
splitSeq 0 _ = []
splitSeq y x = splitAgain y x ++ splitSeq (y-1) x 

splitFinal:: [Int] -> [[Int]]
splitFinal x =[subseq \\ subseq <- removeDup (splitSeq (length x) x)]



f2:: [Int] -> Int
f2 x = length (removeDup (addSeq (splitFinal x)))


addSeq:: [[Int]] -> [Int]
addSeq []  = []
addSeq [x:xs] = [addAllSeq x] ++ addSeq xs

addAllSeq:: [Int] -> Int
addAllSeq x = foldr (+) 0 x


isEqual:: [a] [a] -> Bool | == a
isEqual [] [] = True
isEqual [] xs = False
isEqual ys [] = False
isEqual [x:xs] [y:ys] 
| x <> y = False
| x == y = isEqual xs ys

notIsEqual:: [a] [a] -> Bool | == a
notIsEqual x u = not(isEqual x u)


seqNoDuplicates:: [[Int]] -> [[Int]]
seqNoDuplicates [] = []
seqNoDuplicates [y,x:xs]
| isEqual x (hd xs) = seqNoDuplicates (tl xs)
= [x:seqNoDuplicates xs]


filterQ:: (a a -> Bool) [a] -> [a]
filterQ func [] = []
filterQ func [x] = [x]
filterQ func list=:[x:xs]
| func x x2 = [x: recurse]
| func x x3 = [x: (init xs)]
= recurse
where
  x3 = last xs
  x2 = hd xs 
  recurse = filterQ func xs

remove:: a [a] -> [a] | == a
remove x [] = []
remove x [y:ys] 
| x == y = remove x ys
| otherwise = [y:remove x ys]  

removeDublicates:: [a] -> [a] | == a 
removeDublicates [] = []
// removeDublicates [x:xs]
// | isMember x xs == True = [x:removeDublicates(remove x xs)]
// = [x:removeDublicates xs]



// (quickDelete (splitNew [1,1,2,4,5,3,2,6,3]))


// Function which deletes duplicate and the original of duplicate
// quickDelete:: [[Int]] -> [[Int]] 
// quickDelete [] = []
// quickDelete all=:[first:rest] 
// | isMember first rest =  quickDelete[x \\ x <- rest | notIsEqual first x] 
// = [first: quickDelete rest]


// Function which just deletes duplicate
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

// Start =  splitNew [1,1,2,4,5,3,2,6,3]
// Start = f3 [1,1,2,4,5,3,2,6,3] // [4,5,3,2,6]
// Start = f3 [] // []
// Start = f3 [3] // [3]
Start = f3 [1,1,2,4,5,3,2,6,3,1,2,3,2,4,5,4,6,8,9,12,3,4,5,56,6,7,1,2,3,4,5] // [8,9,12,3,4,5,56,6,7,1,2]




// Start = quickDelete [[1,3],[1..3],[1..2],[1..4], [1,3], [1..4], [1..3], [1..5], [1..4], [1..5]]
// Start = filterQ isEqual [[1..3],[1..3],[1..2]]
// Start = NoDuplicates [[1..2],[1..3],[1..2],[1..3],[1..4]] // [4,5,3,2,6]


// Start = f2 [1..4]
// Start = f2 [] // 0
// Start = f2 [3] // 1
// Start = f2 [1,-3,2,-4,-3,1,7,6,2,8,9] // 34
// Start = f2 [1,1,2,4,5,3,2,6,3,1,2,3,2,4,5,4,6,8,9,12,3,4,5,56,6,7,1,2,3,4,5] // 166
