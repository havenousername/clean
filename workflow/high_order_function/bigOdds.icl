module bigOdds
import StdEnv

bigOdds :: [Int] -> [Int]
bigOdds list  = filter (\item = isOdd item && item > 10) list

// Start = bigOdds [1..20]//[11,13,15,17,19]
// Start = bigOdds [1..100]

// Start = bigOdds [1..20]//[11,13,15,17,19]
// Start = bigOdds [1..100]
// Start = bigOdds []
// Start=bigOdds [1..10]//[]  
Start = bigOdds [12,14..100]//[]