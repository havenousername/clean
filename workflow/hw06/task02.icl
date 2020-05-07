module task02 
import StdEnv

/*
Having a list of tuples, each tuple represent a person in that form (name, age, gender)
Write a function to produce a list of two elements. the older man's name and the older woman's name
i.e : [("Hossam", 19, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 17, "female")] -> ["Hossam", "Nani"]
Note : You can assume that the input for the gender will be "male", "female".
*/

sortByIndex:: [(String, Int, String)] -> [(String, Int, String)] 
sortByIndex [] = []
sortByIndex tList=:[(x,y,z):xs] = sortByIndex [ (x1,y1,z1) \\ (x1,y1,z1) <- xs | y1 < y] ++ [(x,y,z)] ++ sortByIndex [ (x1,y1,z1) \\ (x1,y1,z1) <- xs | y1 >= y]

tuple:: [(String, Int, String)]
tuple = [("Hossam", 19, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 17, "female")]

tuple1:: [(String, Int, String)]
tuple1 = [("Hossam", 19, "male"), ("Evan", 17, "male"), ("Tringa", 18, "female")]

findWoman:: [(String, Int, String)] -> String
findWoman tList=:[(x,y,z): xs] = hd (map (\(x1,y1,z1) = x1) (filter (\(x1,y1,z1) = z1 == "female") (sortByIndex tList)))
// findWoman tList=:[(x,y,z): xs] = last (map (\(x1,x2,x3) = x1) (takeWhile (\(x1,x2,x3) = (x3 == "female") && x1 >= x) tList))

findMan:: [(String, Int, String)] -> String
findMan tList=:[(x,y,z): xs] = hd (map (\(x1,y1,z1) = x1) (filter (\(x1,y1,z1) = z1 == "male") (sortByIndex tList)))
// findMan tList=:[(x,y,z): xs] = takeWhile (\(x1,x2,x3) = x3 == "male" && x1 <= x) tList

findYounger:: [(String, Int, String)] -> [String]
findYounger tList = [findMan tList] ++ [findWoman tList]

// Start = findYounger tuple1
Start = findYounger [("Hossam", 21, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 18, "female")] // ["Hossam", "Tringa"]



// Start = findYounger [("Hossam", 19, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 17, "female")] // ["Hossam", "Nani"]
// Start = findYounger [("Hossam", 19, "male"), ("Evan", 17, "male"), ("Tringa", 18, "female")] // ["Evan", "Tringa"]
// Start = findYounger [("Hossam", 21, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 18, "female")] // ["Hossam", "Tringa"]


