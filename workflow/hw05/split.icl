module split
import StdEnv


split:: Int [Int] -> [[Int]]
split n xs = [take n xs] ++ [drop n xs]

// splitAgain::Int [Int] -> [[Int]]
splitAgain 1 _ = []
splitAgain _ [] = []
splitAgain y xs = [take (y+1) xs] ++ splitAgain (y-1) (tl xs) ++ [drop (y) xs]



length:: [Int] -> Int
length x = foldr (+) 0 (map (\x = 1) x)

splitOne:: [Int] Int -> [[Int]]
splitOne x 0 = []
splitOne x y = split y x ++ splitOne x y


finalSplit:: [Int] -> [[Int]]
finalSplit [] = []
finalSplit x  = splitAgain x ++ splitOne x (length x) 

// finalSplit:: [Int] -> [[Int]]
// finalSplit x  =  splitAgain x (length x)
// finalSplit x  =  splitOne x (length x)



Start = splitAgain 4 [1..4]