module hw08_
import StdEnv


/*
Given a Tree of type Person, return the same tree, except
with "_qualify" attached to the end of the names of each person
who is over 18.
*/

::Person = { name::String,birthday::(Int,Int,Int)}

::Tree a = Node a (Tree a) (Tree a)
	|Leaf

t1::Tree Person
t1 = Node {name = "hh", birthday = (2001,11,22)} Leaf Leaf
t2::Tree Person
t2 = Node {name = "hh", birthday = (2005,11,22)} (Node {name = "hr", birthday = (2001,11,21)} Leaf Leaf)(Node {name = "ht", birthday = (2001,11,23)} Leaf Leaf)
t3::Tree Person
t3 = Node {name = "hh", birthday = (1999,11,22)} (Node {name = "hr", birthday = (2001,11,21)} (Node {name = "hh", birthday = (2003,11,22)} Leaf Leaf) (Node {name = "hh", birthday = (1998,11,22)} Leaf Leaf))(Node {name = "ht", birthday = (2005,11,23)} Leaf Leaf)


//Start = f3 t2 
//(Node (Person "hh" (2005,11,22)) (Node (Person "hr_qualify" (2001,11,21)) Leaf Leaf) (Node (Person "ht" (2001,11,23)) Leaf Leaf))
extractNode:: (Tree Person) -> Person
extractNode (Node x l r) = x 

convertToAge:: Person -> Int 
convertToAge {birthday = b} = 2020 - (fst3 b)

f3:: (Tree Person) -> (Tree Person)
f3 Leaf = Leaf
f3 (Node x=:{name=n, birthday=(y,m,d)} l r)
| (y+18, m, d) < (2020,5,10)  = Node { x&name = (n +++ "_qualify")} (f3 l) (f3 r)
=  Node x (f3 l) (f3 r)

instance < Person 
where
  (<) {birthday = (y1, m1, d1)} {birthday = (y2, m2, d2)} = y1 < y2 || (y1 == y2) && (m1 > m2) || (y1 == y2) && (m1 == m2) && (d1 < d2)   

// instance == Person 
// where
//   (==) {birthday = (y1, m1, d1)} {birthday = (y2, m2, d2)} = y1 == y2 && m1 == m2 && d1 == d2   

// Start = convertToAge (extractNode t1)
// Start = f3 t1
// Start = f3 t2
//(Node (Person "hh" (2005,11,22)) (Node (Person "hr_qualify" (2001,11,21)) Leaf Leaf) (Node (Person "ht" (2001,11,23)) Leaf Leaf))
Start = f3 t3  
//(Node (Person "hh_qualify" (1999,11,22)) (Node (Person "hr_qualify" (2001,11,21)) (Node (Person "hh" (2003,11,22)) Leaf Leaf) (Node (Person "hh_qualify" (1998,11,22)) Leaf Leaf)) (Node (Person "ht" (2005,11,23)) Leaf Leaf))

