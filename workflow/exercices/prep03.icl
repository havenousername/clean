module prep03
import StdEnv

/* Given a list of sublist of Int
   Write ht efunction which contains the minimum of every sublist 
   You should use foldr when finding the minimum 
   Ignore empty sublists
*/

// GetMin:: [[Int]] -> [Int]

minFol:: [Int] -> [Int]
minFol [] = []
minFol [first: rest] = [foldr (\x y = min x y) first rest] 

GetMin:: [[Int]] -> [Int]
GetMin list = flatten [minFol sub \\ sub <- list]
// GetMin [] = []
// GetMin [[]:rest] = [] ++ GetMin rest
// GetMin [first:rest] = minFol first ++ GetMin rest

// Start = minFol [42, 24, -100]

// Start = GetMin [[],[42,24]] // [42, 24]
// Start = GetMin [[]]

TupleSum:: [(Int, Int)] -> (Int, Int)
TupleSum tList = same - diff
where 
  same = sum[(x,y) \\ (x,y) <- tList | isEven x == isEven y]
  diff =  sum[(x,y) \\ (x,y) <- tList | isEven x <> isEven y]

instance + (Int, Int) where + (a,b) (c,d) = (a+c, b+d)
instance - (Int, Int) where - (a,b) (c,d) = (a-c, b-d)
instance zero (Int, Int) where zero = (0,0) 

// Start = TupleSum[(1,2)]
// Start = TupleSum [(1,2),(4,4)]


// condFun:: [((Int -> Bool), Int)] -> [Int]
// condFun biglist = [ sum list \\ (cond,list) <- biglist | and(map cond list)]


// Start = condFun [(isEven, [1..10]), (isOdd, [1,3..7]), (((<) 3), [5..10])]

// Write a fucntion which takes list of numbers and returns list containing only the palidromes
// revInt:: Int -> Int
// revInr x = 

iList:: Int -> [Int]
iList x 
| x < 10 = [x]
= [x  rem 10] ++ iList (x/10) 

isPalindrome:: [Int] -> Bool
isPalindrome list 
| reverse list == list = True
= False


pallist:: [Int] -> [Int]
pallist [] = []
pallist [x:xs] 
| isPalindrome (iList x) = [x: pallist xs]
= pallist xs
// Start = 4545 / 1000

// Start = isPalindrome (iList 12)

// Start  = pallist [12, 11, 1001]

// Start = pallist [76, 89, 1223, 998]
// Start = pallist [33]


lYear :: Int Int -> [Int]
lYear  startX endCount 
| sign startX * sign endCount > 0 = take endCount (dropWhile (\x = x < startX)  (checkL [x \\ x <- [1..] | x rem 4 == 0]))
= []

checkL:: [Int] -> [Int]
checkL [] = []
checkL [x:xs]
| x rem 100 == 0 && x rem 400 == 0 = [x: checkL xs]
| x rem 100 == 0 && x rem 400 <> 0 = checkL xs
= [x: checkL xs]



// Start = lYear -2000 4
// Start = lYear 1999 4

// Start = lYear 1804 7

// Start = lYear 2099 5



// Write a function which takes at integer number and determines whether it is perfect number, or not
// A perfect number is a number which is a sum of all its divisors expept one itself
// isPerfect 6  True

perfectList:: Int -> Int
perfectList number = foldr (+) 0 [x \\ x<- [1..(number-1)] | number rem x == 0]


isPerfect:: Int -> Bool
isPerfect number 
| number > 1 = number == perfectList number
= False

// Start = isPerfect 6 
// Start = isPerfect 496
// Start = isPerfect 11
// Start = isPerfect 0

// Given two integer. Give a list of all common divisors of two integers (expluding one)
divisors:: Int Int -> [Int]
divisors first second 
| first < second  = [ x\\ x <- [2..second] | second rem x == 0 &&  first rem x == 0]
= [ x\\ x <- [2..first] | second rem x == 0 &&  first rem x == 0]


// Start = divisors 6 12
// Start = divisors 7 12
// Start = divisors 9 15
// Start = divisors 128 64
cubeRootTest:: Int -> Bool
cubeRootTest x  = (not(isEmpty([n \\n<- [1..x] | n*n*n == x])))



squareRootTest:: Int -> Bool
squareRootTest x  = (not(isEmpty([n \\n<- [1..x] | n*n == x])))


powerOfTwoTest:: Int -> Bool
powerOfTwoTest x = isMember list
where 
  pow2 = [ a^2\\ a<-[1..]]
  list  = takeWhile ((>=) x) pow2

cubeRoot:: Int -> Int
cubeRoot x = hd [n \\n<- [1..x] | n*n*n == x]

squareRoot:: Int -> Int
squareRoot x = hd [n \\n<- [1..x] | n*n == x]

cubes2:: [Int] -> ([Int], [Int])
cubes2 list = ([cubeRoot x \\ x <- list | cubeRootTest x],[squareRoot x \\ x <- list | squareRootTest x]) 

Start = cubes2 [64, 16, 24, 15, 1, 8]
// Start = squareRoot 8
// cubes2:: [Int] -> ([Int], [Int])
