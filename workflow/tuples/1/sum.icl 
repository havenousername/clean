module sum
import StdEnv

/*Given a list of Int.
Write a function which will calculate the sum of the squares of only the even numbers from that list*/



squares :: [Int] -> [Int]
squares list = [ x^2 \\ x <- list | isEven x]

sumPowerEven::[Int]->Int
sumPowerEven x = foldr (+) 0 (squares x)


// Start = sumPowerEven [1..10]
// Start=sumPowerEven [1..5]//20 =2^2+4^2
// Start=sumPowerEven [2,2,2,2]//16

// Start=sumPowerEven [1,3,5]//0
Start=sumPowerEven []//0
