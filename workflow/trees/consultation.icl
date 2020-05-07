module consultation
import StdEnv

::Tree a = Node a (Tree a) (Tree a) | Leaf



// ::Date = {year:: Int, day:: Int, month:: Int}

// instance < Date
// where 
  // (<) d1 d2 = d1.year < d2.year || (d1.year == d2.year) && (d1.month < d2.month) || (d1.year == d2.year) && (d1.month == d2.month) && (d1.day < d2.day)


// Start =  {year=2000,day=12,month=02} < {year=2000,day=12,month=03}
// dateTree:: (Tree Date)
// dateTree = Node {year = 2000, day = 4, month = 2} (Node {year = 2000, day = 4, month = 2} Leaf Leaf) Leaf

// date1= {year= 2000, month=1, day=3}
// date2= {year= 2001, month=1, day=4}
// date3= {year= 2001, month=2, day=3}
// date4= {year= 2001, month=2, day=4}


// instance toString Date
// where 
  // toString {year=y, month=m, day=d} = "Year=" +++ toString y +++ "Month=" +++ toString m +++"Day=" +++ toString d 


// dateTree3 = Node date3 (Node date1 Leaf (Node date2 Leaf)) (Node date4 Leaf Leaf)

// earliestDate :: (Tree Date) -> String
// earliestDate tree


ourTree :: (Tree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))
//Start = ourTree

messyTree :: (Tree Int)
messyTree = Node 5(Node 12(Node 8 Leaf (Node 1 Leaf Leaf))(Node 6 (Node 9 Leaf Leaf) Leaf))(Node 2 (Node 3 Leaf (Node 13(Node 100 Leaf Leaf)(Node 21 Leaf Leaf)))(Node 40 (Node 60 (Node 70 (ourTree) Leaf) Leaf) Leaf))

// Start  = dateTree

getNode:: (Tree a) -> a
getNode (Node x l r) = x

goL:: (Tree a) -> (Tree a)
goL (Node x l r) = l

goR:: (Tree a) -> (Tree a)
goR (Node x l r) = r

isLeaf:: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False 

getSubtree:: (Tree a) -> [(Tree a)]
getSubtree Leaf = []
getSubtree (Node x l r)  = (getSubtree l) ++ [l,r] ++ (getSubtree r)

// Start = getSubtree ourTree
// Start = isLeaf ourTree
// Start = getNode dateTree

minTree:: (Tree a) -> a
minTree (Node x l r) 
| isLeaf l = x
= minTree l

minTree1:: (Tree a) -> a
minTree1 t
| isLeaf (goL t) = getNode t
= minTree1 (goL t)

minTree2:: (Tree a) -> a
minTree2 (Node x Leaf _) = x
minTree2 (Node _ l _) = minTree l

// Reverse Tree
reverseTree:: (Tree a) -> (Tree a)
reverseTree Leaf = Leaf
reverseTree (Node x l r) = Node x (reverseTree r) (reverseTree l) 

reverseTree1:: (Tree a) -> (Tree a)
reverseTree1 t 
| isLeaf t = Leaf
= Node (getNode t) (reverseTree (goL t)) (reverseTree (goR t))


// Get the max value of BTR
maxTree:: (Tree a) -> a
maxTree t = minTree(reverseTree t)


// getChildren:: a (Tree a) -> (Tree a)
getChildren n Leaf = []
getChildren n (Node x l r)
| x == n = [getNode l, getNode r]
| x < n =  getChildren n l
| x > n = getChildren n r

// Start = getChildren 20 ourTree

// treeToList:: 
treeToList:: (Tree a) -> [a]
treeToList Leaf = []
treeToList (Node x l r) = (treeToList l) ++ [x] ++ (treeToList r)

// inNodeTree:: a (Tree a) -> Bool | Eq a
isNodeTree x tree = isMember x (treeToList tree)

// Start = treeToList ourTree
// Start = isNodeTree 20 ourTree
// Start = maxTree ourTree
// Start = reverseTree1 ourTree


// Create Level balanced function
levelBalance:: [a] -> (Tree a) | Ord, Eq a
levelBalance [] = Leaf
levelBalance list = Node midNode (levelBalance left) (levelBalance right)
where 
  sortedList = sort (removeDup list) 
  len = length  sortedList
  midNode = sortedList!!(len/2)
  left = take (len/2) sortedList 
  right = drop (len/2+1) sortedList 

Start = levelBalance [1..400]