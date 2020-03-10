module split
import StdEnv

splitListEven :: [Int] -> [Int]
splitListEven [] = []
splitListEven [x:xs] 
| isEven x == True = [x] ++ splitListEven xs
= splitListEven xs

splitListOdd :: [Int] -> [Int]
splitListOdd [] = []
splitListOdd [x:xs] 
| isOdd x == True = [x] ++ splitListOdd xs 
= splitListOdd xs

splitList :: [Int] -> [[Int]]
splitList [] = []
splitList [x] = [splitListOdd[x],splitListEven[x]]
// [x \\ x<-[x..(last xs)] | isOdd x] ]

// // Start = splitList [1..12]
// spl :: [Int] -> [Int]
// spl [] = []
// spl [x:xs] = xs

Start = splitList[56,3,87,5,234,5,0,-4]