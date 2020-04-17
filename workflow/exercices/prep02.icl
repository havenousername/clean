module prep02

import StdEnv
/**4
  * Write a function that checks if each elements in the list appear even times.
  
  * For example, checkEven [1,1,2,2,2,2,3,5,3,5] = True
  */

checkEven:: [Int] -> Int
checkEven list = foldr (\x  = (+) 1) 0 [z\\ z <- list | z == hd list]


remove:: Int [Int] -> [Int]
remove x [] = []
remove x [y:ys] 
| x == y = remove x ys
= [y: remove x ys]


checked:: [Int] -> Bool
checked [] = False
checked [x:xs] 
| length ([x:xs]) > 2 = and[(checkEven [x:xs]) rem 2 == 0,  checked (remove x [x:xs])]
= True 


// Start = checked [1,1,2,2,1] // False
// Start = checked [] //False
// Start = checked [1,1,1]    
// Start = checked [1,1,2,2,2,2,3,5,3,5] // True


/**5
  * Write a function that takes two vectors, represented as lists, and returns their dot product.
  
  * The dot product of two vectors can be computed as:
  
  * < xa, xb, xc, ...> * < ya, yb, yc, ...> = (xa*ya) + (xb*yb) + (xc*yc) + ...
  
  * For example: DotProd [4,6,3] [6,3,7] = 24+18+21 = 63
  */


DotProd :: [Int] [Int] -> Int
DotProd [] _ = 0
DotProd _ [] = 0
DotProd [x:xs] [y:ys] = (x * y) + (DotProd xs ys)

// Start = DotProd [4,6] [6,3] 
// Start = DotProd [4,6,3] [6,3,7] //63
// Start = DotProd [6,3,7] [4,6,3] //63
// Start = DotProd [5,2,6,8,3] [5,-8,5,-3,-5] //0

// Start = toInt('1')

// Given a list of characters, split it into a tuple in which the first part only contains digits ('0'..'9'),
// the second part contains the rest. */


l1 :: [Char] ->  [Char]
l1 char = filter (\x = toInt(x) < 58) char 

l2 :: [Char] ->  [Char]
l2 char = filter (\x = toInt(x) >= 58) char 

TwoLists :: [Char] -> ([Char], [Char])
TwoLists char = (l1 char, l2 char)


// Start = TwoLists ['1', 'a', '3', 'b']
// Start = TwoLists  ['1', 'a', '2', 'b', '3'] // (['1','2','3'],['a','b'])
// Start = TwoLists [] // ([],[])

// Given a list of lists, for each list, extract the first, middle and last element. */
Points3 :: [[Int]] -> [(Int, Int, Int)]
Points3 [] = []
Points3 [x] = []
Points3 [x:xs] = [(hd x, last (take (length x / 2 + 1) x), last x)] ++ Points3 xs

// lIn:: [[Int]] -> Int
// lIn [x:xs] = last (take (length x / 2) x)


// Start = Points [[1..9], [1..18]] 
// Start = Points3 [[1..9], [2..6], [3..11], [1..10]] // [(1,5,9),(2,4,6),(3,7,11),(1,6,10)]
// Start = Points3 [[]] //[]




// f8::[(Int,Int,Int)]->[(Int,Int,Int)]

// sortA :: [(Int, Int, Int)] -> [(Int, Int, Int)]
// sortA [(x,y,z)] 
// | x > y && x > z = (x,y, z)
// | y > x && y > z = (x,y, z)

qsort:: [t] -> [t] | Ord t
qsort [] = []
qsort [x:xs] = qsort[sorted \\ sorted <- xs | sorted < x] ++ [x] ++ [sorted \\ sorted <- xs | sorted > x]

qAbs:: [(Int, Int, Int)] ->[(Int, Int, Int)]
qAbs tlist = [(abs(x),abs(y),abs(z)) \\ (x,y,z) <- tlist] 

// tSort:: [(Int, Int, Int)] -> [(Int, Int, Int)]
// tSort [] = []
// tSort [(x,y,z):xs] 
// | x > y && x > z && y > z = [(x,y,z)] ++ xs 
// | y > x && y > z && x > z = [(y,x,z)] ++ xs
// | z > x && z > y && x > y = [(z,x,y)] ++ xs
// | z > x && z > y && y > x = [(z,y,z)] ++ xs
// | y > z && y > x && z > z = [(y,z,x)] ++ xs
// | x > z && x > y && z > y = [(x,z,y)] ++ xs

// Start =  tSort [(3,4,5),(4,5,6),(4,5,3),(6,8,10),(10,5,8),(-3,4,5)]
// Start = abs (~2)
// Start = qAbs [(~1,~2,~4)]



// //f9::[Int] ->Bool
//Use foldr to check if the square root of each integer in a list are all integers. */

f9:: [Real] -> Bool
f9 list = foldr (\x = (toInt(sqrt x) * 2) == x * 2 ) True list

Start = f9 [4.0,16.0,9.0]




