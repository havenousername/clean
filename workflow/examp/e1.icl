module e1
import StdEnv

/**8
//Find the 'unique' right triangle in the list eg. (3,4,5) and (4,3,5) are the same triangle. 
//only one will appear in the answer list [(3,4,5),(4,3,5)] -> [(3,4,5)] */

//f8::[(Int,Int,Int)]->[(Int,Int,Int)]

//Start = f8 [(3,4,5),(4,5,6),(4,5,3),(6,8,10),(10,5,8),(-3,4,5)] //[(3,4,5),(6,8,10)]

//Start = f8 [(1,1,1),(5,4,3),(3,4,5),(0,0,0)] //[(5,4,3)]

*/
toThirdTuple:: [a] -> (a,a,a)
toThirdTuple [x, y, z] = (x, y, z)

sortInsideTuple:: (Int, Int, Int) -> [Int]
sortInsideTuple (x, y, z) = sort[x, y, z]

convertTolist:: [(Int, Int, Int)] -> [[Int]]
convertTolist [] = []
convertTolist [x:xs] = [sortInsideTuple x: convertTolist xs]

convertToThirdTuple:: [Int] -> [(Int, Int, Int)]
convertToThirdTuple [] = []
convertToThirdTuple [x,y,z:xs] = [toThirdTuple [x,y,z]: convertToThirdTuple xs]
  

f8::[(Int,Int,Int)]->[(Int,Int,Int)]
f8 tList = filter (\(x,y,z) = x*x + y*y == z*z && (x > 1 && y > 1 && z > 1)) (convertToThirdTuple (flatten (removeDup (convertTolist tList))))



// Start = f8 [(1,1,1),(5,4,3),(3,4,5),(0,0,0)] //[(5,4,3)]
// Start = f8 [(3,4,5),(4,5,6),(4,5,3),(6,8,10),(10,5,8),(-3,4,5)] //[(3,4,5),(6,8,10)]

//1.
//Define a tree type and use the followings for testing your solution.

tree1 = Node 10 (Node 7 (Node 3 Leaf Leaf) (Node 15 Leaf Leaf)) (Node 5 Leaf (Node 10 Leaf Leaf))
tree2 = Node 9 (Node 1 (Node 0 (Node 7 Leaf Leaf) Leaf) (Node 15 Leaf Leaf)) (Node 4 (Node 4561 Leaf Leaf) (Node 8 (Node 1663 Leaf Leaf) Leaf))
unitTree = Node 1337 Leaf Leaf
noTree = Leaf

:: Tree a = Node a (Tree a) (Tree a) | Leaf

//Write a function that takes a tree as a parameter and returns a list of nodes which have at least one prime child.
//An empty tree will return [].

extractNode:: (Tree Int) -> Int
extractNode Leaf = 4
extractNode (Node x _ _) = x


goL:: (Tree a) -> (Tree a)
goL (Node _ l _) = l

goR:: (Tree a) -> (Tree a)
goR (Node _ _ r) = r

isLeaf:: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False

isPrime:: Int -> Bool
isPrime x = and[(x rem n <> 0)\\ n <- [2..(x-1)]]

treeHasPrime:: (Tree Int) -> Bool
treeHasPrime Leaf = False
treeHasPrime (Node x l r) = or[(isPrime (extractNode l)), (isPrime (extractNode r)), treeHasPrime l, treeHasPrime r]   

// Start = goL tree1 


nodeList:: (Tree Int) -> [Int]
nodeList Leaf = []
nodeList tree 
| isLeaf (goL tree) && treeHasPrime tree = [extractNode tree] ++ nodeList (goR tree)
| isLeaf (goR tree) &&  treeHasPrime tree = nodeList (goL tree) ++ [extractNode tree]
| treeHasPrime tree = nodeList (goL tree) ++ [extractNode tree] ++ nodeList (goR tree)
=  nodeList (goL tree) ++ nodeList (goR tree)

// Start = nodeList noTree

// Start = isPrime (extractNode (goL tree1))
// Start = isPrime 12
// Start = isPrime 12


//2.
//Given a tuple of arrays, representing sets of integers, return a list containing the result of their symmetric-difference.
//The symmetric-difference between two sets is equivalent to the difference between their union and their intersection.
arr1:: {Int}
arr1 = {1,2,3,4}

arr2:: {Int}
arr2 = {2}

simpleTuple:: ({Int}, {Int})
simpleTuple = (arr1, arr2)

removeCons :: [a] -> [a] | Eq a //generalizing the function 
removeCons [] = []
removeCons [x] = [x]
removeCons [a,c:b] 
| a == c = removeCons(dropWhile (\x = x==a) [c:b])
= [a: removeCons [c:b]]  

symmetricDiff1:: ({Int}, {Int}) -> [Int]
symmetricDiff1 (arr1, arr2) = removeCons( sort([x \\x <-:arr1 ] ++ [z \\ z <-:arr2]))

symmetricDiff:: ({Int}, {Int}) -> [Int]
symmetricDiff (arr1, arr2) = [z \\ z <-: arr1 | not (isMember z l2)] ++ [z \\ z <-: arr2 | not (isMember z l1)]
where
  l1 = [z \\ z <-: arr1]
  l2 = [z \\ z <-: arr2]

// Start = symmetricDiff ({1,2,3,4},{3,4,5,6})
// Start = symmetricDiff ({1,2,3,4},{-2,-4,13,0}) //[1,2,3,4,-2,-4,13,0]
// Start = symmetricDiff ({1,2,3,4},{1,2,3,4}) //[]
// Start = symmetricDiff ({},{1,2,3,4}) //[1,2,3,4]


//3.
//Define a Q type for rational numbers.
//Write a function that receives two fractions and calculates their division. Simplify the fraction before returning.
//In case the nominator is zero, set the denominator to zero as well.

::Q = {nom:: Int, den:: Int}

q1:: Q
q1 = {nom=2, den=4} 

q2:: Q
q2 = {nom=1, den=4} 

simplify :: Q -> Q
simplify {nom = a, den= b}
| b == 0 = abort "Division by zero error"
| b < 0 = simplify {nom= -1*a, den = -1*b }
= {nom = a / g, den = b/ g}
where 
  g = gcd a b

instance zero Q
where
  zero = {nom = 0, den = 1}
instance / Q 
where
  (/) {nom = a1, den = b1} {nom = a2, den = b2} = {nom = a1 * b2, den = b1 * a2}

mkQ:: Int Int -> Q
mkQ n d = simplify {nom = n, den = d}
 
divide:: Q Q -> Q
divide x1 x2 
| (x1 / x2).nom == 0 = zero
= simplify (x1 / x2)  
// Start = simplify {nom = 4, den = 2}

instance toInt Q
where 
  toInt {nom = a, den = b} 
  | b == 1 = a
  = abort "No integer analog\n" 

instance fromInt Q
where
  fromInt i = mkQ i 1

// Start = toInt (divide q1 q2)
// Start :: Q
// Start = fromInt (toInt (divide q1 q2))

// Start = divide {nom=5, den=1} {nom=6, den=5} //{nom=25, den=6}

half = { nom=1, den=2 }
third = { nom=1, den=3 }
fourth = { nom=1, den=4 }
fifth = { nom=1, den=5 }
sixth = { nom=1, den=6 }
threehalf = { nom=3, den=2 }
twothird = { nom=2, den=3 }
ninefourth = { nom=9, den=4 }
threefifth = { nom=3, den=5 }

miniTree = Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf)			
smallTree = Node half (Node fourth Leaf Leaf) (Node ninefourth Leaf Leaf)
bigTree = Node half (Node fifth (Node sixth Leaf Leaf)(Node third (Node fourth Leaf Leaf) Leaf))(Node threehalf (Node threefifth Leaf (Node twothird Leaf Leaf))(Node ninefourth Leaf Leaf))
badTree = Node third (Node fourth Leaf Leaf)(Node ninefourth (Node sixth Leaf Leaf) Leaf)


//4.
//Write a function that will return the sum of a tree's nodes.
//Return the sum as a simplified Q.

instance + Q
where
  (+) {nom = n1, den = d1} {nom = n2, den = d2} = simplify{nom = (n1 * d2) + (n2 * d1), den = d1*d2 }

sumFromTree:: (Tree Q) -> Q
sumFromTree Leaf = zero
sumFromTree (Node x l r) = x + (sumFromTree l) + (sumFromTree r)

// Start = sumFromTree miniTree
// Start = sumFromTree smallTree 

//5.
//Write a function that will check if a tree of Q is a Binary Search Tree.

treeToList:: (Tree a) -> [a]
treeToList Leaf = []
treeToList (Node x l r) = treeToList l ++ [x] ++ treeToList r 

isBinaryTree:: (Tree a) -> Bool | Ord, Eq a
isBinaryTree tree = (treeToList tree) == sort (treeToList tree) 

instance == Q
where
  (==) q1=:{nom = n1, den = d1} q2=:{nom = n2, den = d2} = (simplify q1).nom == (simplify q2).nom && (simplify q1).den == (simplify q2).den

instance < Q 
where 
  (<) {nom = n1, den= d1} {nom = n2, den= d2} 
  | d1 <> d2 = (n1 * d2) < (n2 * d1)
  = n1 < n2 

// Start = treeToList miniTree

// Start = half == half
// Start = isBinaryTree miniTree
// Start = isBinaryTree badTree //True


:: Color = Red | Yellow | Green | Blue | Purple | Violet
:: ColorCombo = { color1 :: Color, color2 :: Color}

instance == Color
where
    == Red Red = True
    == Yellow Yellow = True
    == Green Green = True
    == Blue Blue = True
    == Purple Purple = True
    == Violet Violet = True
    == _ _ = False

colorList :: [Color]
colorList = [Red,Yellow,Green,Blue,Purple,Violet]

//6.
//Write a function that when given a color, returns its complement.
//That is:
//Red -> Blue, Yellow -> Purple, Green -> Violet, Blue -> Red, Purple -> Yellow, Violet -> Green

takeComplement1:: Color [Color] -> Color
takeComplement1 col l=:[x:xs]  
| col == x = l!!3
= takeComplement1 col xs

getIndex:: Color [Color] Int -> Int
getIndex col [x:xs] y
| col == x = y
= getIndex col xs (y+1)

complement:: Color -> Color
complement col 
| getIndex col colorList 0 < (length colorList) / 2 = takeComplement1 col colorList
= takeComplement1 col (reverse colorList)


// Start = complement Blue
// Start = complement Green //Violet

// EVAN SOLUTION 
/*
This solution utilizes a technique called mapping
in which I used a list to map the colors to integers,
which are conveniently provided by the list's indices.
*/


colorComp:: Color -> Color
colorComp clr = newColor
where
  index = hd[i\\ n <- colorList & i<-[0..] | n == clr]
  newClrIndex = (index + 3) rem 6
  newColor = colorList!!newClrIndex


// Start = colorComp Purple
// complement:: Color -> Color
// complement 

//7.
//Write a function that when given a Color, creates a list of possible color combos.
//Valid color combos can not have duplicate colors.

combos:: Color [Color] -> [ColorCombo]
combos clr clist = recordList
where
  recordList = filter (\{color1=c1,color2=c2} = c1 <> c2) (map (\x = {color1=clr,color2=x}) clist )

colorCombo:: Color -> [ColorCombo] 
colorCombo clr = combos clr colorList 

// Start = combos Red colorList
// Start = colorCombo Blue //[{color1=Blue, color2=Red},{color1=Blue, color2=Yellow},{color1=Blue, color2=Green},{color1=Blue, color2=Purple},{color1=Blue, color2=Violet}]

// Evan Implementation

clrCombo:: Color -> [ColorCombo]
clrCombo c = combo
where
  index = hd[i\\ n <- colorList & i <- [1..] | c == n]
  listFiltered = [n \\ n <- colorList & i <- [1..] | i <> index]
  combo = [{color1 = c, color2 = x} \\ x <- listFiltered]


// Start = clrCombo Blue

//8.
//Amicable numbers are two different numbers so related that the sum of the proper divisors of each 
//is equal to the other number. (A proper divisor of a number is a positive factor of that number other than the number itself. 
//For example, the proper divisors of 6 are 1, 2, and 3.) 
//Check if two integers are amicable pairs and put them together with the answer in a bag.
//amicable pair: 1184 and 1210 
//proper divisor of 1184 :  1, 2, 4, 8, 16, 32, 37, 74, 148, 296, 592 -> their sum == 1210
//proper divisor of 1210 : 1, 2, 5, 10, 11, 22, 55, 110, 121, 242, 605 -> their sum == 1184

divisorList:: Int -> [Int]
divisorList x = [n \\ n <- [1..(x-1)] | x rem n == 0]


::Bag a :==[((Int, Int), Bool)]

amiBag :: [(Int, Int)] -> Bag a
amiBag [] = []
amiBag [(f, s):xs] 
| sum (divisorList f) == s && sum (divisorList s) == f = [((f,s), True): amiBag xs]
=  [((f,s), False): amiBag xs]


amiBagEvan :: [(Int, Int)] -> Bag a
amiBagEvan l = [((a,b), (sum (divisorList a) == b && a == sum (divisorList b))) \\ (a ,b) <-l]

// Start = amiBagEvan [(1184,1210), (13,245)]

// Start = divisorList 1184


//9.
//Given the Object type, compute for the state component the given method and print the result as a String.
//ex: for state 3 compute 3 + 1 using the given method, and print the result "4" as string.

:: Object = {state::Int, method::Int->Int, tostring:: Int -> String }

MyObject = {state=3,method = (+) 1,tostring = toString}

// Start = MyObject.method 

//10.
//Given an operator and two lists, apply the operator to the elements of 
//the same positions of lists, like in the examples.

::Operator a :== a a -> a

op2 :: (Operator a) [a] [a] -> [a]
op2 op l1 l2 = [op x y \\ x <- l1 & y <- l2]

// Start = op2 (*) [2,3,4,5] [7,8,9,10] // [14,24,36,50]
// Start:: [Int]
// Start = op2 (*) [] [] // []
// Start = op2 (+) [3,2] [7,8] // [10,10]


// Start = [1,2,3,4,5 :[3]]
// h  x y _ = y

:: Treea a b = Nodea a (Treea a) (Treea a) | Leafa b

Start = Nodea 1 Leafa "H2"