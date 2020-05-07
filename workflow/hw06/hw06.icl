module hw06
import StdEnv

/*
Given a list of pairs of name of the person and his/her favourite food.
Make function which returns list of pairs of food and a list of people who likes it.
Note : order doesn't matter
*/

genList:: [(String, String)] -> [String]
genList tList = map (\(x, y) = y) tList

remove:: (String, [String]) [(String, [String])] -> [(String, [String])]
remove x  [] = []
remove x [(y1, y2):ys]
| fst x == y1 && length (snd x) > length y2 = remove x ys
= [(y1, y2):remove x ys]

removeTuple:: [(String, [String])] -> [(String, [String])]
removeTuple  []  = []
removeTuple [(x, y): ys] = [(x,y): removeTuple (remove (x,y) ys)]


removeDupTuple::[(String, [String])] -> [(String, [String])]
removeDupTuple [] = []
removeDupTuple [(x, list)] = [(x, list)]
removeDupTuple [(x, list):xs]
| x == fst (hd xs) && length list > length (snd (hd xs)) = [(x, list): (removeDupTuple (drop 1 xs))]
= [(x, list): removeDupTuple xs]


favFood:: [(String, String)]  -> [(String, [String])]
favFood [] = []
favFood tList=:[(x, y):xs] = removeTuple ([(y, map (\(x,y) = x) (filter (\(z,zy) = zy == y) tList))] ++ favFood xs)

// searchFor str tList = [(x, y) \\ (x, y) <- tList |  y == str]
// favFood:: [(String, String)] -> [(String, [String])]
// favFood tList = searchFor tList
// favFood tList = filter (\(x,y) = x == genList )


// Start = favFood [("Zuka", "apple"), ("Beka", "orange"), ("Emad", "pineapple"), ("Ahmed", "apple")] // [("apple", ["Zuka", "Ahmed"]),("orange",["Beka"]),("pineapple",["Emad"])]
// Start = favFood [("Zuka", "apple"), ("Beka", "orange"), ("Emad", "pineapple"), ("Ahmed", "pineapple")] // [("apple", ["Zuka"]),("orange",["Beka"]),("pineapple",["Emad","Ahmed"])]



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
// Start = findYounger [("Hossam", 21, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 18, "female")] // ["Hossam", "Tringa"]



// Start = findYounger [("Hossam", 19, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 17, "female")] // ["Hossam", "Nani"]
// Start = findYounger [("Hossam", 19, "male"), ("Evan", 17, "male"), ("Tringa", 18, "female")] // ["Evan", "Tringa"]
// Start = findYounger [("Hossam", 21, "male"), ("Nikola", 21, "male"), ("Tringa", 18, "female"), ("Nani", 18, "female")] // ["Hossam", "Tringa"]




/*
Decide if a number is triangular number and write the count of levels of triangle. 
Triangular number is a number that can form a triangle.
The output should be in a tuple.
Note : if it is false the count should be -1. 
examples:
1       3         6          10          15
                                          *
                               *          * *
                  *            * *        * * *
        *         * *          * * *      * * * *
*       * *       * * *        * * * *    * * * * *
Note : 0 is not a triangular number
*/

triangleNum:: Int -> Int
triangleNum 0 = 0
triangleNum x = x + triangleNum (x-1)

triangleList:: Int -> [(Int,Int)]
triangleList xs = [(triangleNum x, x)\\ x <- [1..xs]]

isTringularNum:: Int -> (Bool,Int)
isTringularNum xs = triangleRes (filter (\(x,y) = x == xs) (triangleList xs))

triangleRes:: [(Int, Int)] -> (Bool, Int)
triangleRes [] = (False, -1)
triangleRes [(x,y)] = (True, y)

// Start = isTringularNum -1 // (False,-1)
// Start = isTringularNum 1 // (True,1)
// Start = isTringularNum 5 // (False,-1)
// Start = isTringularNum 10 // (True,4)
Start = isTringularNum 666 // (True,36)
// Start = isTringularNum 3
// Start = isTringularNum 0 // (False,-1)
