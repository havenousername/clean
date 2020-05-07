module p
import StdEnv

// Mafia:: [(String, Int, Bool)]
// Mafia = ["Kaluletti",]
// list :: [Int]
// list = [1..100]


::SomeName = { arg1 :: Int
  , arg2 :: Bool
  }

func:: SomeName -> Bool
func x = x.arg2


::Rec1 = { val1 :: Bool
  , val2 :: Int
 }

::Rec2 = { arg1 :: Rec1
 , arg2 :: Int
  }

func:: Rec2 -> Int
func {arg1 = a, arg2 = b}
| b rem 2 == 1 = b
= a.val2

Start = func {arg1 = {val1 = True, val2 = 5}, arg2 = 6}

// Start = func {arg1=5,arg2=False}

// binary_search:: Int [Int] -> Int
// binary_search y list
// | x == 