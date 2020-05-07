module trees
import StdEnv

::Month = Jan | Feb | Mar | Apr | May 

func :: Month -> Int
func x = 1


::Date = {
            day::Int,
            month::Int,
            year::Int
          }

::Status = On Date | Off


checkStatus:: Status -> String
checkStatus (On x) = "This is on"
checkStatus Off =  "This is off"

p1:: Status
p1 = On {year= 2000, month=01, day=22}
p2 = Off

// Start = checkStatus p1

:: Tree a = Node a (Tree a) (Tree a) | Leaf
// ::Tree = Node Tree Tree | Leaf
// ::Tree = Node [Tree] | Leaf
// ::Tree a = Node a (Tree a) (Tree a) | Leaf 

/*
     8 
   /  \
  6    2
 / \  / 
2  2  2   
*/

// Binary Search Tree -> BST 
smallTree:: (Tree Int)
smallTree = Node 8 (Node 6 (Leaf) (Leaf)) (Node 12 (Leaf) (Leaf)) 


extractNode:: (Tree a) ->  a
extractNode (Node x l r) = x
extractNode Leaf = abort "Leaf encountered\n"


treeToList:: (Tree a) -> [a]
treeToList Leaf = []
treeToList (Node x l r) =  (treeToList l)++ [x] ++ (treeToList r)

Start = treeToList smallTree // [6,8,12]

// Start = extractNode Leaf
// Start = 