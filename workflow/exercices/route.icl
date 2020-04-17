module route
import StdEnv

/**1
  * Write a function, that takes a list of functions, and a list of
  * tuples (Int, Int) where the first Int indicates which function to
  * use and the second Int acts as a parameter and returns a list of
  * the results.
  
  * For example: Router [isEven,isOdd] [(1,2),(2,4),(1,57)] = [True, False, False]
  */
  

  
// route:: a [(a -> b)] [(Int,a)] -> [b] 
// route 0 _ _ = []
// route i [] list = []
// route i function [] = []
// route i function [(x,y):xs]
// | x == i = [(last function) y] ++ (route (i) (init function) xs)
// = route (i-1) function [(x,y):xs] 





// routeParent:: [(a -> b)] [(Int,a)] -> [b]
// routeParent y z = route (2) y z

// route:: [(a -> b)] [(Int,a)] -> [b]
// route func=:[y:ys] tup = map changer list
// where 
//   list = snd (unzip tup) 

// routeChild:: [(a->b)] (Int,a) -> [b]
// routeChild [] tuple = []
// routeChild listOfFunc tuple
// | (fst tuple) == length listOfFunc = [(last listOfFunc) (snd tuple)]
// = (init listOfFunc) tuple
  
// routeChild:: [(a->b)] -> [b]
// routeChild a = init a

// Start = routeParent [isEven, isOdd]
// Start = routeChild [isEven, isOdd] 5
// Start = routeChild [isEven, isOdd] (1,2)
// Start = routeParent [isEven,isOdd] [(1,2),(2,4),(1,57)]
// Start = take 1 [isEven,isOdd] 
// Start  = testT [(1,2),(2,4),(1,57)]


/**2
  * Write a function that takes a list of integers and returns a list of
  * result integers based on how many integers were in the parameter list.
  * For 1 integer 'a', it will return that integer modulus 2. (a rem 2)
  * For 2 integers 'a','b' , it will return a list of all integers from the first to the second. [a..b]
  * For 3 integers 'a','b','c' , it will return (a*(b^c))
  * For 4 integers 'a','b','c','d', it will return a list of the sum of 'a' and 'b' and the sum of 'c' and 'd'.
  */

listing:: [Int] -> [Int]
listing [] = [] 
listing [x] = [x rem 2]
listing list=:[x:xs] 
| (length list) == 2 = [ return \\ return <- [x..(last xs)]]
|  (length list) == 3 =  [x * ((hd xs) ^ (last xs))]
|  (length list) == 4 =  [ x + (hd xs), last (take 2 xs) + (last xs) ]

// Start = listing [2,5]
// Start = listing [5] //[1]
// Start = listing [4,10] //[4,5,6,7,8,9,10]
// Start = listing [3,5,2] //[75]
// Start = listing [13,29,1030,307] //[42,1337]
// Start = listing [] //[]


