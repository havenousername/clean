module pr01
import StdEnv

:: BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Leaf

newBinaryTree:: BinaryTree Int
newBinaryTree = Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 8 (Node 5 Leaf Leaf) (Node 9 Leaf Leaf)) 

ourTree :: (BinaryTree Int)
ourTree = Node 15(Node 3(Node 1 Leaf Leaf)(Node 10(Node 7 Leaf (Node 8 Leaf Leaf))(Node 13 (Node 11 Leaf Leaf) Leaf)))(Node 20 (Node 18 Leaf (Node 19 Leaf Leaf)) (Node 21 Leaf (Node 26 (Node 24 Leaf Leaf) (Node 28 Leaf Leaf))))

messyTree :: (BinaryTree Int)
messyTree = Node 5(Node 12(Node 8 Leaf (Node 1 Leaf Leaf))(Node 6 (Node 9 Leaf Leaf) Leaf))(Node 2 (Node 3 Leaf (Node 13(Node 100 Leaf Leaf)(Node 21 Leaf Leaf)))(Node 40 (Node 60 (Node 70 (ourTree) Leaf) Leaf) Leaf))

BinaryTreeToList:: (BinaryTree a) -> [a]
BinaryTreeToList Leaf = []
BinaryTreeToList (Node x l r) = BinaryTreeToList l ++ [x] ++ BinaryTreeToList r

ListToBinaryTree:: [a] -> (BinaryTree a) | Ord, Eq a
ListToBinaryTree [] = Leaf
ListToBinaryTree [x:xs] =  (insertToBinaryTree x) (ListToBinaryTree xs)  

insertToBinaryTree:: a (BinaryTree a) -> (BinaryTree a) | Ord a
insertToBinaryTree a Leaf = Node a Leaf Leaf
insertToBinaryTree a (Node x l r) 
| a <= x = Node x (insertToBinaryTree a l) r
| a > x = Node x l (insertToBinaryTree a r)

extractNode:: (BinaryTree a) -> a
extractNode (Node x l r) = x  

goLeft:: (BinaryTree a) -> (BinaryTree a)
goLeft (Node x l r) = l


goRight:: (BinaryTree a) -> (BinaryTree a)
goRight (Node x l r) = r


//Checking if we're at a leaf
isLeaf:: (BinaryTree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False

//Get a list of subtrees from a node.
getTreeList:: (BinaryTree a) -> [(BinaryTree a)]
getTreeList Leaf  = []
getTreeList (Node x l r) = getTreeList l ++ [(Node x Leaf Leaf)] ++ getTreeList r 

getSubtree:: (BinaryTree a) -> [(BinaryTree a)]
getSubtree (Node x l r) = [l, r] 


//Get the min value of a BST
minBinaryTree:: (BinaryTree a) -> a | Ord a 
minBinaryTree tree
| isLeaf (goLeft tree) = extractNode tree 
= minBinaryTree (goLeft tree)  

//Reverse a tree
reverseBinaryTree:: (BinaryTree a) -> (BinaryTree a)
reverseBinaryTree Leaf = Leaf
reverseBinaryTree (Node x l r) = Node x (reverseBinaryTree r) (reverseBinaryTree l) 

//Get the max value of a BST
maxBinaryTree:: (BinaryTree a) -> a | Ord a
maxBinaryTree tree
| isLeaf (goRight tree) = extractNode tree
= maxBinaryTree (goRight tree)

maxBinaryTree1:: (BinaryTree a) -> a | Ord a
maxBinaryTree1 tree = minBinaryTree (reverseBinaryTree tree)

subTreeList:: (BinaryTree a) ->[(BinaryTree a)]
subTreeList Leaf = []
subTreeList tree = subTreeList (goLeft tree) ++ [tree] ++ subTreeList (goRight tree) 
//Extract sublists countaining a specific element
extractSublistFromBinaryTree:: a (BinaryTree a) -> [(BinaryTree a)] | Eq a
extractSublistFromBinaryTree y tree = [ subTree \\ subTree <- (subTreeList tree) | extractNode subTree == y ]


// Start = subTreeList newBinaryTree
// Start = extractSublistFromBinaryTree 3 ourTree
//Get a list of children of a node
getChildren:: a (BinaryTree a) -> [a] | Eq a
getChildren n tree
| isLeaf(goLeft subTree) && isLeaf(goRight subTree) = []
| isLeaf(goLeft subTree) = [extractNode(goLeft subTree)] 
| isLeaf(goRight subTree) = [extractNode(goRight subTree)]
= [extractNode (goLeft subTree)] ++ [extractNode (goRight subTree)]
where
  subTree = hd(extractSublistFromBinaryTree n tree)

getParent:: Int (BinaryTree Int) -> Int
getParent n tree 
| isEmpty (findParentAux n tree) = 0
= hd(findParentAux n tree) 

findParentAux n tree 
| isLeaf tree = []
| isMember n (getChildren (extractNode tree) tree) = [extractNode tree]
| n == extractNode tree = abort "No parent\n"
= findParentAux n (goLeft tree) ++ findParentAux n (goRight tree)


// Check if a Binary Tree is actually a BST
// checkBST:: (BinaryTree a) -> Bool | Ord a
checkBST tree = sort list == list
where
    list = BinaryTreeToList tree




//Add a new node to a BST
addNodeToBinary:: a (BinaryTree a) -> (BinaryTree a) | Ord , Eq a
addNodeToBinary n Leaf = Node n Leaf Leaf 
addNodeToBinary n (Node x l r)
| n == x = (Node x l r)
| n < x = Node x (addNodeToBinary n l) r 
| n >x = Node x l (addNodeToBinary n r)  

// Start = getParent 13 ourTree
// Start = getParent 2 Leaf

//Remove the minimum node of a binary search tree
removeMinBinary:: (BinaryTree a) -> (BinaryTree a) | Ord, Eq a
removeMinBinary (Node x Leaf r) = r
removeMinBinary (Node x l r)
| extractNode l == minBinaryTree l = Node x (goRight l) r
= Node x (removeMinBinary l) r  

//Remove the root of a binary search tree
removeRootBinary:: (BinaryTree a) -> (BinaryTree a) | Ord, Eq a
removeRootBinary (Node x l r) 
| isLeaf l && isLeaf r =  Leaf
| isLeaf l = r
| isLeaf r = l
= Node (minBinaryTree r) l (removeMinBinary r) 


//Filter a binary search tree
filterBinaryTree:: (a -> Bool)  (BinaryTree a) -> (BinaryTree a) | Ord, Eq a
filterBinaryTree _ Leaf = Leaf
filterBinaryTree f (Node x l r) 
| f x = Node x (filterBinaryTree f l) (filterBinaryTree f r)
= filterBinaryTree f (removeRootBinary (Node x l r)) 


// Binary tree to List with AUX function
binaryTreeToList:: (BinaryTree a) -> [a]
binaryTreeToList tree 
| isLeaf tree = []
= leftTree ++root++rightTree
where
  leftTree = binaryTreeToList (goLeft tree)
  rightTree = binaryTreeToList (goRight tree) 
  root = [extractNode tree] 

//Reorganize a messy tree into a BST
reorganize:: (BinaryTree a) -> (BinaryTree a) | Ord, Eq a
reorganize tree = balanceBinaryTree (binaryTreeToList tree)

balanceBinaryTree:: [a] -> (BinaryTree a) | Ord, Eq a
balanceBinaryTree [] = Leaf
balanceBinaryTree x = Node root (balanceBinaryTree left) (balanceBinaryTree right)
where 
  sortedList = sort(removeDup x)
  len = length sortedList
  root = sortedList!!(len/2)
  left = take (len/2) sortedList
  right = drop (len/2+1) sortedList 

minMess:: (BinaryTree a) -> a | Ord, Eq a
minMess tree = minBinaryTree (reorganize tree)

maxMess:: (BinaryTree a) -> a | Ord, Eq a
maxMess tree = maxBinaryTree (reorganize tree)

Start = maxMess messyTree


// Start = filterBinaryTree (\x = x > 5) newBinaryTree
// Start = removeRootBinary newBinaryTree
// Start = removeMinBinary newBinaryTree
// Start = addNodeToBinary 10 newBinaryTree
// Start = checkBST newBinaryTree

// Start = getChildren 20 ourTree
// Start = reverseBinaryTree newBinaryTree
// Start = minBinaryTree newBinaryTree
// Start = isLeaf newBinaryTree
// Start = getSubtree newBinaryTree
// Start = insertToBinaryTree 1 newBinaryTree
// Start = ListToBinaryTree [3,5,2,4]
// Start = goRight newBinaryTree
// Start = extractNode newBinaryTree
// Start = getTreeList newBinaryTree
// Start = maxBinaryTree1 newBinaryTree
// Start = extractSublistFromBinaryTree 3 ourTree
// Start = getChildren 20 ourTree