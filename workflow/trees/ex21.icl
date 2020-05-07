module ex21
import StdEnv

:: C = { re :: Real
       , im :: Real
       } 


mkC r i = { re = r, im = i } 

// Start = mkC 1.0 10.0 // (C 1 10)

instance + C 
where
  (+) x y = mkC (x.re + y.re) (x.im + y.im)

// Start = mkC 2.2 4.1 + mkC 1.5 6.4 // (C 3.7 10.5)

 
instance - C
where 
    (-) x y = mkC (x.re-y.re) (x.im-y.im)

//Start = mkC 2.2 4.1 - mkC 1.5 6.4 // (C 0.7 -2.3)


instance * C
where 
    (*) x y = mkC (x.re*y.re - x.im*y.im) (x.re*y.im + x.im*y.re)

// Start = mkC 2.0 4.0 * mkC 3.0 2.0 // (C -2 16)


// for simplicity only division by a real nr. is defined
instance / C
where 
    (/) x y 
    | y.im == 0.0 = mkC (x.re/y.re) (x.im/y.re)
    = abort "division not defined"

// Start = (mkC 2.0 4.0) / (mkC 2.0 0.0) // (C 1 2)


instance fromReal C
where 
    fromReal r = mkC r 0.0
    
//Start :: C
//Start = fromReal 3.0 // (C 3 0)


instance toReal C
where 
    toReal x
    | x.im == 0.0 = x.re
    = abort "x has imaginary part"
    
//Start = toReal (mkC 3.0 0.0) // 3


instance zero C
where 
    zero = fromReal 0.0

//Start :: C   
//Start = zero // (C 0 0)


instance one C
where 
    one = fromReal 1.0

//Start :: C   
//Start = one // (C 1 0)


instance abs C
where 
    abs x = fromReal (sqrt (x.re*x.re + x.im*x.im))
    
//Start = abs (mkC 3.0 4.0) // (C 5 0)


//conjugate of a complex x+yi is x-yi
instance ~ C
where 
    (~) x = mkC x.re (~x.im)

//Start = ~ (mkC 2.0 3.0) // (C 2 -3)


instance toString C
where
    toString x
        | x.im == 0.0 = toString x.re
        | otherwise = toString x.re +++ "+" +++ toString x.im +++ "i"
        
// Start = toString (mkC 3.0 4.0) // "3+4i"


instance == C
where 
    (==) x y = x.re == y.re && x.im == y.im
    
//Start = mkC 1.0 2.0 == mkC 1.0 2.0  // True

// tests whether the complex number represents a real nr.
isRealC :: C -> Bool    
isRealC x 
| x.im == 0.0 = True 
= False
 
//Start = isRealC (mkC 2.0 0.0) // True


// returns real part
re :: C -> Real
re x = x.re

//Start = re (mkC 1.0 2.0) // 1


// returns imaginary part
im :: C -> Real
im x = x.im

// Start = im (mkC 1.0 2.0) // 2


// Exercises:
/*
Define a binary tree type and using that solve 1 to 6.
*/

::Tree a = Node a (Tree a) (Tree a) | Leaf

newTree :: Tree Int
newTree =  Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 8 (Node 5 Leaf Leaf) (Node 9 Leaf Leaf)) 


unbalancedTree:: Tree Int
unbalancedTree = Node 5 (Node 2 Leaf Leaf) Leaf

// 1. Count the number of leaves in a binary tree.

countNodes :: (Tree a) -> Int
countNodes Leaf = 0
countNodes (Node x l r) = 1 + (countNodes l) + (countNodes r)

countLeaves :: (Tree a) -> Int
countLeaves Leaf = 1
countLeaves (Node x l r) = 0 + (countLeaves l) + (countLeaves r)

// Start = countNodes newTree

// 3. Check if a binary tree is balanced (the difference between the depth of left and right 
// trees is at most 1)

extractNode:: (Tree a) -> a
extractNode (Node x l r) = x

goL:: (Tree a) -> (Tree a)
goL (Node x l r) = l

goR:: (Tree a) -> (Tree a)
goR (Node x l r) = r 

isLeaf:: (Tree a) -> Bool
isLeaf Leaf = True
isLeaf _ = False

isBalanced:: (Tree a) -> Bool
isBalanced Leaf = False
isBalanced (Node x l r)
| isLeaf r && isLeaf l = True 
| isLeaf r = False
| isLeaf l = True
= (isBalanced l) && (isBalanced r)

// Start = isBalanced newTree
// 4. Check if a binary tree is ordered (i.e. has the property of a binary search tree 
// with smaller elements then root to left and bigger elements to left)

treeToList:: (Tree a) -> [a] | Ord, Eq a
treeToList Leaf = []
treeToList (Node x l r) = (treeToList l) ++ [x] ++ (treeToList r)

isOrdered:: (Tree a) -> Bool | Ord, Eq a
isOrdered Leaf = True
isOrdered tree = (treeToList tree) == sort (treeToList tree)

// instance == [a]
// where 
  // (==) [] [] = [] == []

Start = isOrdered Leaf

// 5. listDescending: returns the elements in descending order of magnitude, starting at
// the greatest element.
// Note: do not implement listDescending as reverse of traversal of inorder of a tree.

listDescending:: (Tree a) -> [a]
listDescending Leaf = []
listDescending (Node x l r) = (listDescending r) ++ [x] ++ (listDescending l)

// Start = listDescending newTree

// 6. listToLeaves: returns the elements in order of distance to the root of the tree,
// starting at the root element, then all elements below (left to right), then all
// elements below those (left to right), etc.
// hasParent:: (Tree a) -> Bool | Eq a
// hasParent tree = root <> extractNode tree
// where 
  // list = (treeToList tree)
  // len = length list
  // root = (treeToList tree)!!(len/2)

// listToLeaves:: (Tree a) -> [a] | Eq a
// listToLeaves Leaf = []
// listToLeaves (Node x l r) 
// | hasParent (Node x l r) == False = [x, extractNode l,  extractNode r ] ++ (listToLeaves l) ++ (listToLeaves r)
// = [extractNode l, extractNode r] ++ (listToLeaves l) ++ (listToLeaves r)


// Start = listToLeaves newTree
// Start = listToLeaves newTree
// instance + [a]
// where
  // (+) [x:xs] [y:ys] = [sum (x y), (+) xs ys] 

// Start = [1,2] + [4,5]
