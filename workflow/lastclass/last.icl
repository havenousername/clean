module last
import StdEnv


/*
  today we will cover:
  -Arrays
  -Type Aliasing
  -Instances
*/


something:: {Int}
something = {1,2,3,4}

// Start = something.[3] // gets us the index 3 element

// with an array you cant use  .. constructor

// Start = [1..]

// Start = {1,3..}  cant do that

// if it is error we declare Start type
// Start :: {Int}
// Start = {1,2,3,4,5}

// arrays can not use : constructor
// [a:b] [a,b,c:d]

// arrays can use , constructor and the  \\ constructor

bruh:: {Int}
bruh = { x \\ x <-[1..100] | isOdd x }

// Start = bruh

lol:: {Int}
lol = { x \\ x <-: bruh | x > 50}

// Start =  size lol

// [1,2,3,4,5] -> n 
// {1,2,3,4,5} -> 1

// String is actually is an array of Characters #{Char}

myName:: String
myName = "Andrei"

initial :: String -> Char
initial x = x.[0]

//Start = initial myName // 'E'

// Start = size myName
// Start :: {Int}
// Start = {}

reverseString:: String -> String
reverseString x = res
where
  l = [ elem \\ elem <-: x]
  m = reverse l
  res = {ch \\ ch <- m}

// Start = reverseString myName

// given an array of strings, filter out the one that are palindromes

myArr :: {String}
myArr = {"racecar", "hello", "mmmmmmmmmmmmm", "blah", ""}

// Start = myArr.[1].[(size myArr.[1]) - 1]

isPal :: String -> Bool
isPal x = l == reverse l
where 
  l = [e \\ e <-: x]

palList:: {String} -> {String}
palList myArr = { aString \\ aString <-: myArr| isPal aString && size aString < 10 && size aString > 0 }


// not(isPal x) == (not o isPal) x

isPalidrome :: Int -> Bool
isPalidrome x = l == reverse l
where 
  arr = toString x
  l = [e \\ e<-: arr ]


// Start = isPalidrome 1221
// Start = palList myArr

numDigits:: Int -> Int
numDigits x = size (toString x) 

// Start = numDigits 234323

/* 
    End or arrays
 */ 

// ----------------
// TYPE ALIASES

::MyType :== Int

func :: MyType -> MyType
func  x = inc x 

func2 :: MyType -> Bool
func2  x = x > 0 

//Start = func 2

:: StupidType :== (Int, String)

func3 :: StupidType -> String
func3 (a,b) = b

// Start = func3 (2323, "hello")

// String :== #{Char} 


//  ============================
// INSTANCES

:: Date = {day:: Int, month:: Int, year:: Int}

days:: Date
days = {day = 1, month = 1, year = 2000}

daya:: Date
daya = {day = 1, month = 1, year = 2000}

dayz:: Date
dayz = {day = 3, month = 2, year = 2000}

listOfDays:: [Date] 
listOfDays = [daya, days, dayz]

// This is no the best option (because built function use == not equals)
(equals) :: Date Date -> Bool
(equals) {day = d1, month = m1 , year = y1} {day = d2, month = m2 , year = y2} = and[d1 == d2, m1 == m2, y1 == y2]

instance == Date 
where
  (==) {day = d1, month = m1 , year = y1} {day = d2, month = m2 , year = y2} = and[d1 == d2, m1 == m2, y1 == y2]

instance < Date 
where
  (<) {day = d1, month = m1 , year = y1} {day = d2, month = m2 , year = y2} = or[ y1< y2, and[y1==y2, m1 < m2], and[y1==y2, m1 < m2, d1 == d2]]

// What needs sort?
// sort    ::               !u:[a] -> u:[a] | Ord a


// Start = days equals daya
// Start = isMember days listOfDays

Start = sort listOfDays


// What class does?
// Automatization -> classes
/*

class Ord a | < a
where
	/**
	 * Greater than.
	 * 
	 * @result True iff the first value is strictly greater than the second value.
	 */
	(>) infix 4 :: !a !a -> Bool | Ord a
	(>) x y :== y < x

	/**
	 * Smaller than or equal to.
	 * 
	 * @result True iff the first value is smaller than or equal to the second value.
	 */
	(<=) infix 4 :: !a !a -> Bool | Ord a
	(<=) x y :== not (y < x)

	/**
	 * Greater than or equal to.
	 * 
	 * @result True iff the first value is greater than or equal to the second value.
	 */
	(>=) infix 4 :: !a !a -> Bool | Ord a
	(>=) x y :== not (x < y)

	/**
	 * The minimum of two values.
	 */
	min :: !a !a -> a | Ord a
	min x y :== case (x < y) of
		True -> x
		_ -> y

	/**
	 * The maximum of two values.
	 */
	max :: !a !a -> a | Ord a
	max x y :== case (x < y) of
		True -> y
		_ -> x



*/