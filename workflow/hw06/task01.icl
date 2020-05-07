module task01
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
Start = favFood [("Zuka", "apple"), ("Beka", "orange"), ("Emad", "pineapple"), ("Ahmed", "pineapple")] // [("apple", ["Zuka"]),("orange",["Beka"]),("pineapple",["Emad","Ahmed"])]
