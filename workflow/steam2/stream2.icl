module stream2
import StdEnv


// Lets do a complex filter function
// Start = filter isEven [1..10]

// Start = [ x \\ x <- [1..10] | isEven x]

// Start = [ x \\ x <- [1..10] | isEven x && ((<)4) x]
// Start = [ x \\ x <- [1..10] | isEven x || ((<)4) x]
// Start = [ x \\ x <- [1..10] | and[isEven x, ((<)4) x]] // a && b -> and[a,b]
// Start = [ x \\ x <- [1..10] | or[isEven x, ((<)4) x]] // a || b -> or[a,b]

isPrime:: Int -> Bool
isPrime x = and[ x rem y <> 0 \\ y <- [2..(x-1)]]

filterSum:: (Int -> Bool) String (Int -> Bool) [Int]  -> [Int] 
filterSum predOne "and" predTwo list = filterSumAux predOne and predTwo list
filterSum predOne "or" predTwo list = filterSumAux predOne or predTwo list

filterSumAux:: (Int -> Bool) ([Bool] -> Bool) (Int -> Bool) [Int]  -> [Int] 
filterSumAux predOne op predTwo list = [ x \\ x <- list | op[predOne x, predTwo x]]


// Start = filterSum isEven "and" isPrime [1..10]

fibX:: Int -> Int 
fibX a = fibAux a 0 1

fibAux:: Int Int Int -> Int
fibAux x a b 
| x == 0 = a 
= fibAux (x-1) b (a+b)


fibList:: Int -> [Int]
fibList x = takeWhile ((>=) x) [fibX n \\ n <-[1..]]

Start = fibList 999999999999999999900