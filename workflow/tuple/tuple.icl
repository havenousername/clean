module tuple
import StdEnv

/*
  Tuple is a way a contain and transport different types of data
  Lists can only contain elements of the same type: ["Jogj", "Gloty"] [23,23,45]
  (123, "James") :: (String, Int)
*/

// Common use of the tuple - Distionary(cpp - maps)

Bank:: [(String, Int)]
Bank = [("Omen", 12889), ("Vase", 10), ("Olea", 200), ("Olge", 0)]

getAccountBalance:: String -> Int
getAccountBalance name = snd(hd [ individualAccount \\ individualAccount <- Bank | fst individualAccount == name])

// Start = getAccountBalance "Omen"

updateAccount:: (String, Int) -> (String, Int, Bool)
updateAccount (_, 0) = ("Broke faggot", 0, False)
updateAccount (name, balance)
| balance < 0 = ("Broke faggot", 0, False)
= (name, balance, balance > 200)

isValid:: (String, Int, Bool) -> Bool
isValid ( _, _, True) = True
isValid ( _, _, False) = False

// updateAccount account = (fst account, snd account, (snd account) > 200)

eligiblePeople:: [(String, Int)] -> [String]
eligiblePeople ourBank = result 
where 
  updatedAccounts = map updateAccount ourBank
  filteredAccounts = filter (\(_,_, status) = status) updatedAccounts
  accountNames = map (\(name,_,_) = name) filteredAccounts
  result = accountNames


eligiblePeople1:: [(String, Int)] -> [String]
eligiblePeople1 ourBank = [ name \\ (name, _, status) <- (map updateAccount ourBank) | status]

Start = eligiblePeople1 Bank


// ( a, b, c)


// funct:: ipadress -> (status, (status report))
//  10.0.0.0            (offline, (10.0.0.0 node 1))
//  (10,0,0,0)
//  (Int,Int,Int,Int) -> (Bool, ((Int, Int, Int, Int) String))
