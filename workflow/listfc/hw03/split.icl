module split
import StdEnv

splitList :: [Int] -> [[Int]]
splitList a = [[ x \\ x <- a | isOdd x]] ++ [[ x \\ x <- a | isEven x]]


// Start = splitList[420]
Start = splitList [56,3,87,5,234,5,0,-4]

