module e3
import StdEnv

//// Records

:: Point = {  x       ::  Real
            , y       ::  Real
            , visible ::  Bool
            }

:: Vector = { dx       ::  Real
            , dy       ::  Real
            }
  
Origo :: Point
Origo = { x = 0.0
        , y = 0.0
        , visible = True
        }
Dist :: Vector
Dist = { dx = 1.0
       , dy = 2.0
       }

IsVisible :: Point -> Bool
IsVisible {visible = True} = True
IsVisible _                = False

xcoordinate :: Point -> Real
xcoordinate p = p.x

hide :: Point -> Point
hide p = { p & visible = False }

Move :: Point Vector -> Point
Move p v = { p & x = p.x + v.dx, y = p.y + v.dy } 

// Start = Move (hide Origo) Dist

//// Trees

:: Tree a = Node a (Tree a) (Tree a)
          | Leaf


// Start = ourTree

treesort :: ([a]-> [a]) | Eq, Ord a
treesort = collect o listtoTree

listtoTree :: [a] -> Tree a | Ord, Eq a
listtoTree [] = Leaf
listtoTree [x:xs] = insertTree x (listtoTree xs)

insertTree :: a (Tree a) -> Tree a | Ord a
insertTree e Leaf = Node e Leaf Leaf
insertTree e (Node x le ri)
   | e<=x = Node x (insertTree e le) ri
   | e>x  = Node x le (insertTree e ri)

collect :: (Tree a) -> [a]
collect Leaf = []
collect (Node x le ri) = collect le ++ [x] ++ collect ri

// Start = treesort [3, 1, 5, 9, 2, 7, 0]

list:: [Int]
list = [3, 1, 5, 9, 2, 7, 0]

// Exercises

// 1. Compute the sum of the numbers placed in the nodes of a tree.
sumT :: (Tree Int) -> Int
sumT Leaf = 0
sumT (Node x l r) = x + (sumT l) + sumT r


// Start = sumT (listtoTree list)

// 2. Test about 3 points if they can form a right-angled triangle.
IsTriangle :: Point Point Point -> Bool
IsTriangle {x=x1,y=x2} {x=y1,y=y2} {x=z1,y=z2} = x2 == y2 && y1 == z1

// Start = IsTriangle {x=2.0, y=1.0, visible = True} {x=4.0, y=1.0,  visible = True} {x=4.0, y=4.0, visible = True} 


// 3. Write another sort algorithm for sorting a list
quickSort:: [Int] -> [Int]
quickSort [] = []
quickSort [x:xs] = quickSort[n \\ n <- xs | n <= x] ++ [x] ++ quickSort[n \\ n <- xs | n > x]


// Start = quickSort list

// 2. generate the following list [(1,1),(1,2),(2,1),(2,2)]
l2 :: [(Int, Int)]
l2 = [(x,y)\\ x <- [1..2], y <- [1..2]]

// 4. generate the list [(1,5),(2,6),(3,7),(4,8),(5,9),(6,10)]
l4 :: [(Int, Int)]
l4 =  [ (x,y)\\ x <- [1..6] & y <- [5..10]]

// 5. generate the list [1,2,2,3,3,3,4,4,4,4,...,10,..,10]
l5 :: [Int]
l5 = flatten [take x (repeat y) \\ y <- [1..10] & x <- [1..10]] 

// 6. generate the list [[1],[2,2],[3,3,3],[4,4,4,4],...,[10,..,10]]
l6 :: [[Int]]
l6 = [take x (repeat y) \\ y <- [1..10] & x <- [1..10]] 

// 7. generate 6 pythagoras numbers : [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20)]
l7 :: [(Int, Int, Int)]
l7 = take 6 [(x, y, z) \\ x <- [2..100] , y <-[2..100] , z <-[2..100] | (x*x+y*y) == z*z]

// Start = l7


/*
1. Given a tree and an integer n, find the nodes equal to n and replace by '0'
*/


// :: Tree a = Node a (Tree a) (Tree a)
//           | Leaf

// atree = (Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 6 (Node 3 Leaf Leaf) (Node 7 Leaf Leaf)))

// f1 :: Int (Tree Int) -> (Tree Int)
f1 _ Leaf = Leaf
f1 n (Node x l r) 
| n == x = Node 0 (f1 n l) (f1 n r)
=  Node x (f1 n l) (f1 n r)

// Start = f1 3 atree

/*
2. Given a tree, find the level between max node and min node
*/

btree = Node 4 (Node 2 (Node 1 Leaf Leaf)(Node 3 Leaf Leaf)) (Node 6 (Node 5 Leaf Leaf)(Node 7 Leaf Leaf))
ctree =  Node 4 (Node 2 (Node 8 Leaf Leaf)(Node 9 (Node 4 (Node 16 Leaf Leaf) Leaf) Leaf)) (Node 7 (Node 3 Leaf Leaf)(Node 2 Leaf Leaf))

// goL:: (Tree a) -> (Tree a)
extractNode:: (Tree a) -> a
extractNode (Node x _ _) = x
// f2 :: (Tree Int) ->Int
// f2 
convertToList:: (Tree Int) -> [Int]
convertToList Leaf = []
convertToList (Node x l r) = [x] ++ convertToList l ++ convertToList r

getMaxTree :: (Tree Int) -> Int
getMaxTree tree = maxList (convertToList tree)

getMinTree :: (Tree Int) -> Int
getMinTree tree = minList (convertToList tree)


treeFindIndex:: Int (Tree Int) Int -> [Int]
treeFindIndex _ Leaf _ = []
treeFindIndex item (Node x l r) ind 
| x <> item =  treeFindIndex item r (ind+1) ++ treeFindIndex item l (ind+1)
= [ind + 1]


dif:: (Tree Int) -> Int
dif tree =(hd (treeFindIndex (getMaxTree tree) tree 1) ) - (minList (treeFindIndex (getMinTree tree) tree 1) )
 
// Start = dif ctree
// Start = dif btree

/*
3. 
Define algebraic type : Day (Mon,Tue,Wed,Thu,Fri,Sat,Sun).
And define function IsWeekend :: Day -> Bool to check if it is Sat or Sun.
if it is weekend, then output "Happy day!",Otherwise,"Oh my god"
*/

:: Day = Mon | Tue | Wed | Thu | Fri | Sat | Sun

IsWeekend :: Day -> Bool
IsWeekend day 
| day == Sat || day == Sun = True
= False

instance == Day
where 
  (==) Mon Mon = True 
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _ = False

f3 :: Day -> String
f3 day 
| IsWeekend day = "Happy day!"
= "Oh my god"

// Start = f3 Sun  // "Happy day!"
// Start = f3 Tue  // "Oh my god"


//// Records

:: Person = { name :: String
            , birthdate :: (Int,Int,Int)
            , fpprogramer :: Bool
            }

IsfpUser :: Person -> String
IsfpUser {fpprogramer = True} = "Yes"
IsfpUser _                    = "No"

// Start = IsfpUser { name = "Me"
//                 , birthdate = (1,1,1999)
//                 , fpprogramer = True}    // "Yes"



GetName :: Person -> String
GetName p = p.name

GetName2 :: Person -> String
GetName2 {name} = name

ChangeN :: Person String -> Person
ChangeN p s = {p & name = s} 


atree = Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)

nrNodes :: (Tree2 a) -> Int
nrNodes (Leaf2 y) = 1
nrNodes (Node2 x l r) = 1 + nrNodes l + nrNodes r

::Tree2 a = Node2 a (Tree2 a) (Tree2 a) | Leaf2 a

aTree2 :: Tree2 Int
aTree2 = Node2 4 (Node2 2 (Node2 1 (Leaf2 1) (Leaf2 1)) (Node2 3 (Leaf2 3) (Leaf2 3))) (Leaf2 5)

:: Tree3 a b = Node3 a (Tree3 a b) (Tree3 a b)
             | Leaf3 b

aTree3 :: Tree3 Int Real

aTree3 = Node3 2 (Node3 1 (Leaf3 1.1) (Leaf3 2.5)) (Node3 3 (Leaf3 3.0) (Leaf3 6.9))

// Start = atree
// Start = aTree2
// Start = aTree3

sumLeaves :: (Tree3 Int Real) -> Real
sumLeaves (Leaf3 y) = y
sumLeaves (Node3 x le ri) = sumLeaves le + sumLeaves ri

//Start = sumLeaves aTree3 //13.5

// Triple branches
:: Tree4 a = Node4 a (Tree4 a) (Tree4 a) (Tree4 a)
          | Leaf4

// Rose-tree - tree with variable multiple branches
// No leaf constructor, node with no branches
:: Tree5 a = Node5 a [Tree5 a]

// Every node has one branch = list
:: Tree6 a = Node6 a (Tree6 a) 
           | Leaf6


// Tree with different types
:: Tree7 a b = Node7a Int (Tree7 a b) (Tree7 a b)
             | Node7b b (Tree7 a b)
             | Leaf7a b
             | Leaf7b Int

:: BTree a				= Bin (BTree a) (BTree a) | Tip a


mapBTree:: (a -> b) (BTree a) -> (BTree b)
mapBTree fun (Tip a) = Tip (fun a)
mapBTree fun (Bin l r) = Bin (mapBTree fun l) (mapBTree fun r)

foldrBTree:: (a a -> a) (BTree a) -> a
foldrBTree f (Tip x) = x
foldrBTree f (Bin l r) = f (foldrBTree f l)  (foldrBTree f r)


aBTree = Bin (Bin (Bin (Tip 1) (Tip 1) ) (Bin (Tip 3) (Tip 3))) (Tip 5)

tipTree = Tip 3
// Start = mapBTree inc aBTree
// Start = foldrBTree (+) aBTree // 13

// Exercises

// 1. Finish the implementation of the Stack 

getElements:: Int (BTree a) -> [a]
getElements x (Tip y)
| x == 0 = [y]
| x > 0 = []  
getElements x (Bin l r) 
| x == 0 = abort "No elements in the Tip. Increase your index\n"
= (getElements (x-1) l) ++ (getElements (x-1) r)

pushElement:: a (BTree a) ->  (BTree a)
pushElement x (Tip y) = Bin (Tip y) (Tip x)
pushElement x (Bin l r) = Bin (pushElement x l) (pushElement x r) 

// Start =pushElement 2333 (pushElement 1223122 aBTree)

isTip:: (BTree a) -> Bool
isTip (Tip x) = True
isTip _ = False

popElement::(BTree a) -> (BTree a)
popElement (Bin r l)
| isTip l = r
= Bin (popElement l) r 

// Start = popElement (popElement aBTree)

//Going down left/right subtree
goL :: (Tree a) -> (Tree a)
goL (Node x l r) = l
goR :: (Tree a) -> (Tree a)
goR (Node x l r) = r

//Checking if we're at a leaf
isLeaf :: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False

//Create a list of all subtrees
subTreeList :: (Tree a) -> [(Tree a)]
subTreeList Leaf = []
subTreeList tree = subTreeList(goL tree) ++ [tree] ++ subTreeList(goR tree)

removeVowels:: String -> String
removeVowels str = {ch\\ ch <-: str | ch <> 'a' && ch <> 'A' && ch <> 'e' && ch <> 'E' && ch <> 'i' && ch <> 'I' && ch <>'o'&& ch<>'O' && ch <> 'u' && ch <> 'U' }
// || ch == 'A' || ch == 'e' || ch == 'E' || ch == 'i' || ch == 'I' || ch =='o' || ch=='O' || ch == 'u' || ch == 'U'}


// Start = removeVowels "ALoo"

::Vector3 a = {x0 :: a, x1:: a, x2::a}

instance == (Vector3 a) | == a
where
  (==) {x0 = p1, x1 = p2, x2 = p3 } {x0 = v1, x1 = v2, x2= v3 } = (p1 == v1) && (p2 == v2) && (p3 == v3) 


instance zero (Vector3 a) | zero a
where
  zero = {x0=zero, x1=zero,x2=zero}

instance one (Vector3 a) | one a
where
  one = {x0=one, x1=one,x2=one}  

instance + (Vector3 a) | PlusMin a 
where 
  (+) {x0 = p1, x1 = p2, x2 = p3 } {x0 = v1, x1 = v2, x2= v3 } = {x0 = p1+v1,x1 = p2+v2, x2 = p3 + v3}

v1 :: (Vector3 Int) 
v1 = {x0 = 2, x1 = 3, x2 = 4}

v2 :: (Vector3 Int)
v2 = {x0 = 2, x1 = 3, x2 = 4}


ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
// Start:: (Vector3 Int)
// Start = one + v1

dotProduct:: (Vector3 a) (Vector3 a) -> a | *, + a 
dotProduct {x0 = p1, x1 = p2, x2 = p3 } {x0 = v1, x1 = v2, x2= v3 } = p1 * v1 + p2 * v2 + p3 * v3


// :: Tree a = Node a (Tree a) (Tree a) | Leaf

remove:: a [a] -> [a] | Eq a
remove _ [] = []
remove y [x:xs]
| y == x = xs 
= [x: (remove y xs)]

treeToList:: (Tree a) -> [a]
treeToList Leaf = []
treeToList (Node x l r) = treeToList l ++ [x] ++ treeToList r

listToTree:: [a] -> (Tree a)
listToTree [] = Leaf
listToTree [x:xs] = Node x (listToTree xs) Leaf  
where 
  l = sort list


levelBalance :: [a] -> (Tree a) | Ord, Eq a
levelBalance list
| isEmpty list = Leaf
= (Node (sorted!!mid) (levelBalance (take mid sorted)) (levelBalance (drop (mid+1) sorted)))
    where
        sorted = sort (removeDup list)
        mid = (length sorted)/2


Start = levelBalanced (remove 8 (treeToList ourTree))

// Start = dotProduct v1 v2

/// 
 


//Extract sublists countaining a specific element
// extractSubLists :: a (Tree a) -> [(Tree a)] | Eq a
// extractSubLists n tree = [subtree\\subtree<-(subTreeList tree)|(extractNode subtree)==n]
// Start = extractSubLists 3 ourTree






