module prep01
import StdEnv

// 1. Write a function that will return the second to last digit in a number. Return 0 if there is no second digit.

//f1 :: Int -> Int

//Start = f1 1234 //3

//Start = f1 5 //0

//Start = f1 ~(5564) //6

f1:: Int -> Int
f1 x = ((abs(x) / 10) rem 10)


// Start = f1 5 //0
// Start = f1 (~(5564)) //6
// Start = ((~(5564) ) / 2) *2
// Start = f1 1234


// 2. Write a function that will subtract numbers in a list from the first one. Your solution must use 'foldr' or 'foldl'.
// Return 0 for an empty list.

f2 :: [Int] -> Int
f2 [] = 0
f2 [x:xs]  = foldl (-) x xs

// Start = f2 [10,1,2,3] //4
// Start = f2 [1,2,3,4] //-8
// Start = f2 [1000,500,250,125] //125
// Start = f2 [] //0


// 3. Write a function that returns all prime divisors of a number. e.g. f3 36 = [1,2,3]

isPrime:: Int -> Bool
isPrime x = and[x rem y <> 0 \\ y <- [2..(x-1)]]

f3:: Int -> [Int]
f3 x  =  [ n \\ n <- [1..x] | (x rem n) == 0 && isPrime n]


Start = [ x\\ x <- [1..100] | isPrime x]

// Start = f3 0 //[]
// Start = f3 524287  //[1,524287]
// Start = divisors 36


// 4. Write a function that reverses tuples from a list if the tuple members sum up to an even number.

// f4:: [(Int, Int)] -> [(Int, Int)]
// f4 list = map  list

tupleSum:: (Int, Int) -> Int
tupleSum (x,y) = x+y


f4:: [(Int, Int)] -> [(Int, Int)]
f4 [] = []
f4 [(x,y): xs]
| isEven (tupleSum (x,y)) = [(y,x): f4 xs] 
= [(x,y): f4 xs] 

// Start = tupleSum (7,5)
// Start = (snd (7,5), fst (7,5)) 
// Start = tupleSomeReverse [(1,2),(3,4),(5,7)] //[(1,2),(3,4),(7,5)]
// Start = f4 [(-1,3),(12,1),(0,0),(-4,-2)] //[(3,-1),(12,1),(0,0),(-2,-4)]
// Start = f41 [(1,2),(3,4),(5,7)] //[]

// 5. Write a function that takes every number in a list and generates a sublist of its first 5 multiples. Your solution must use 'map'.

someFunc:: Int -> [Int]
someFunc x 
| x > 0 = take 5 [ y\\ y <- [1..] | (y rem x) == 0 ] 
= take 5 [(~y)\\ y <- [1..] | (y rem x) == 0 ] 

f5 :: [Int] -> [[Int]]
f5 list = map  someFunc list
// f5 list = map (\x =  take 5 [ y\\ y <- [1..] | (y rem x) == 0 ]) list


// Start = f5 [1..3] //[[1,2,3,4,5],[2,4,6,8,10],[3,6,9,12,15]]
// Start = f5 [4,~3,5,~6] //[[4,8,12,16,20],[-3,-6,-9,-12,-15],[5,10,15,20,25],[-6,-12,-18,-24,-30]]
// Start = [(-1)..(-100)]
// Start = f5 [] //[]

// 6. Given an integer n, find the minimal k such that
// k = m! (where m! = 1 * 2 * ... * m) for some integer m; k >= n.

fac:: Int Int -> Int
fac x y = facAux x y 1

facAux:: Int Int Int -> Int
facAux x y counter 
| counter > y  = counter 
= facAux (x+1) y (counter*x)


leastfactorial :: Int -> Int
leastfactorial 1 = 1
leastfactorial x = fac 1 x

// Start = leastfactorial 1


// 7. Write a function that checks if a list of numbers is odd,even,odd,even...

// e.g. for [1,2,3,4,6] it is false because 4 is even, but 6 is not odd


f7 :: [Int] -> Bool
f7 [] = False
f7 [x] = isOdd x
f7 xs 
| (length xs) rem 2 == 0 = isEven (last xs) && (f7 (init xs))
| (length xs) rem 2 <> 0 = isOdd (last xs) && (f7 (init xs))


// Start = f7 [1..10]
// Start = f7 [1,3,4,5] //False
// Start = f7 [1,2,3,4,6,7] //False
// Start = f7 [] //False
// Start = f7 [1,2,3] //True
// Start = f7 [2,3,4] //False

// 8. Write a function that removes consecutive duplicates in a list.

//f8 :: [Int] -> [Int]


// f8:: [Int] -> [Int]
// f8 [] = []
// f8 list=:[x,y:xs]
// | x == y =  f8 (removeAt x [y:xs]) 
// = [x:f8 [y:xs]]
// test:: a a -> Bool
// test a b = a == b 

// f8:: [a] -> [a] | Eq a
// f8 [] = []
// f8 [x:xs] = [x:dropWhile (test x) xs]


f8 :: [a] -> [a] | Eq a //generalizing the function 
f8 [] = []
f8 [x] = [x]
f8 [a,c:b] 
| a == c = f8(dropWhile (\x = x==a) [c:b])
= [a: f8 [c:b]]  

// Start = f8 [1,1,3,4,1,1]
// Start = dropCons [4,5,6,6,8,2,2,2,4,0,0,0,7,0,5,0,0,4]//[4,5,8,4,7,0,5,4]
// Start = f8 [4,5,6,6,8,2,2,2,4,0,0,0,7,0,5,0,0,4]
// Start = f8 [1,0,0,2,0,3,3,0,6,7,0,7,7] //[1,2,0,0,6,7,0]
// Start = f8 [2,0,0,6,7,5,0,8,0,5,0,0,0] //[2,6,7,5,0,8,0,5] 


// 9. Write a function that takes a tuple of three lists and generates a list of triple tuples.

// The triple tuple is only generated if the i-th member of the first list multiplied by the

// i-th member of the second list equals the i-th member of the third list.

// e.g. for ([1,2,3,4,5],[2,4,6,8,10],[2,8,17,32,45]) the result is [(1,2,2),(2,4,8),(4,8,32)]

zip3::([Int],[Int],[Int])->[(Int,Int,Int)]
zip3 ([], y ,z) =[]
zip3 (x ,[] ,z) = []
zip3 (x ,y ,[]) = []
zip3 ([],[],z) = []
zip3 (x,[],[]) = []
zip3 ([],y,[]) = []
zip3 ([x:xs],[y:ys],[z:zs]) 
| x*y == z = [(x,y,z): zip3 (xs,ys,zs)]
= zip3 (xs,ys,zs)


// Start = zip3 ([2,2,2,2,2,2],[1,2,3,4,5,6,7,8],[2,4,6,6,10]) //[(2,1,2),(2,2,4),(2,3,6),(2,5,10)]
// Start = zip3 ([1,2,3,4,5],[2,4,6,8,10],[2,8,1,32,45])//[(1,2,2),(2,4,8),(4,8,32)]
// Start = zip3 ([1,0,1,0,1,0],[3,4,5,6,8],[3,0,5,0,0])//[(1,3,3),(0,4,0),(1,5,5),(0,6,0)]

// 10. Write a function that checks if a number is a Mersenne Prime.
// A Mersenne Prime is a prime number that is 1 less than a power of 2. example: 7 = (2^3) - 1 = 8-1

twoPower:: Int -> [Int]
twoPower x = takeWhile (\y = y <= x^2) [ 2^n \\ n <- [1..]]



f10:: Int -> Bool
f10 x = isMember (x+1) (twoPower x)

// Start = f10 7

// Start =  f10 (0)









