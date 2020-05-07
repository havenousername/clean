module tuples
import StdEnv

/*
Tuples are a way of put things together easily

*/

listOne = [1112, 2332, 344,2334]
listTwo = ["James", "Ajck", "Napie"]


james:: (String, Int) 
james = ("James",122)

// Start = fst james // "james"
// Start = snd james // 122
// Start = (listOne, listTwo)
// fst and snd work only for tuples of 2 elements

// Start = thd3 ("Janek", 22, "programmer")
// Start = ("jamek") // singleton tuples dont exist in Clean

funcOne:: Int Int -> (Int, Int)
funcOne x y = (x,y)
// Start = funcOne 2 3 

// Start = [ (a,b)\\ a<- [1..7], b <- [a..7] | a+b == 7]


func3 :: Int -> [(Int, Int, Int, Bool)]
func3 n = [ (a,b, b-2, isEven a  && isEven b)\\ a<- [1..n], b <- [a..n] | a+b == n]


// Start = func3 100

// dictionary, map , key , pairings

Bank:: [(String, Int)]
Bank = [("Josh",467 ), ("Kajcj",46000 ), ("Evan",-46000), ("Evan", 30042)]


lookUpBank:: String [(String, Int)] -> [Int]
lookUpBank name accountsList = [ accountBalance \\ (accountName, accountBalance) <- accountsList | accountName == name]
// lookUpBank name accountsList = snd (hd [ account \\ account <- accountsList | fst account == name])

// Start = lookUpBank "Evan" Bank
updateAccount::  [(String, Int)] -> [(String, Int, Bool)]
updateAccount l = [ ( a, b, b > 0) \\  (a,b) <- l ]

// Start = updateAccount Bank  // [("Josh",467,True),("Kajcj",46000,True),("Evan",-46000,False),("Evan",30042,True)]
stimulus:: [(String, Int, Bool)] -> [(String, Int, Bool)]
stimulus accountsList = map (\(a,b,c) | c == False = (a, b+1000 , True) = (a,b,c)  )  accountsList
// stimulus accountsList = map (\account | thd3 account == False = (fst3 account, (snd account)+1000 , True ) = account  )  accountsList

// Start = stimulus( updateAccount Bank)

