module q1
import StdEnv

/*
Grade 2 : Please write a function to decide if its input is Integer or not. (No type signature needed)
Grade 3 : Modify your function to decide if it its input is natural number.
Grade 4 : Please write a function to decide if a number is a perfect square.
Grade 5 : Please write a function to decide if a all the numbers in a list are perfect square.
INFO : Perfect square number is an integer equals to a square of another integer.
i.e : 9 is a perfect square since 3^2 = 9
*/

isNat x = toReal (toInt(x)) ==  toReal(x)

isNatural x = and[(toInt(x) >= 0), isNat x]


// Start = isNatural -1
// Start = isNat 1.1 // False
// Start = isNat 4 // True
// Start = isNat 4.1 // True
// Start = isNat -1 // False

numberSq = [ (num)^2 \\ num <- [1..] ]

// isPerfectSquare1 :: Int -> Bool
// isPerfectSquare1 x 
// | x > 0 = isMember x numberSq
// = False

isPerfectSquare :: Int -> Bool
isPerfectSquare x = isNat(sqrt (toReal x))  



// instance sqrt (Int -> Real)

// Start = isNat 3
// Start = isPerfectSquare 9
//isPerfectSquare :: Int -> Bool
// Start = isPerfectSquare 9 // True
// Start = isPerfectSquare 1 // True
// Start = isPerfectSquare 0 // True
// Start = isPerfectSquare 17 // False
// Start = isPerfectSquare -1 // False

areAllPerfectSquares :: [Int] -> Bool
areAllPerfectSquares  squares = and[ isPerfectSquare(sq) \\  sq <- squares]
// | isPerfectSquare x = areAllPerfectSquares xs 
// | isPerfectSquare x == False = False 

// Start = areAllPerfectSquares [1, 9, 36, 16] // True
// Start = areAllPerfectSquares [2, 4, 34] // False



isDiv a b = b rem a == 0

// Start = isDiv 2 6

allDiv s = [x\\ x <- [1..s] | s rem x == 0]

commonDiv a b 
| a > b = [x \\ x <- [1..a] | a rem x == 0 && b rem x == 0 && isPrime x ]
= [x \\ x <- [1..b] | a rem x == 0 && b rem x == 0 && isPrime x ]

commonDivA a b 
| a > b = [x \\ x <- [1..a] | a rem x == 0 && b rem x == 0 ]
= [x \\ x <- [1..b] | a rem x == 0 && b rem x == 0 ]

isPrime n = and[ n rem x <> 0 \\ x <- [2..(n-1)]]


// Write a function which counts number of digits in a number

// count:: Int Int -> Int
// count x y
// | x rem 10 == x = y
// = count (x/10) (y+1)  

countNum:: Int -> Int
countNum x 
| x < 0 = 0
= count x 1
 where 
  count x y
  | x rem 10 == x = y
  = count (x/10) (y+1) 

// Start = countNum 2134

// Start = countNum 12

// Start = count 20 1


isMagicNum :: Int -> Bool
isMagicNum n
| n == 0 = False
| n < 10 = True
= isMagicNum (n/10) && n rem (countNum n) == 0


// Start = isMagicNum 118

areAllMagicNumbers:: [Int] -> Bool
areAllMagicNumbers x = and(map (\n = isMagicNum n) x)

// Start = allDiv 6
// Start = commonDiv 6 12
// Start = commonDivA 6 12

// Start = [1..10]
Start = areAllMagicNumbers [4,51,120]

