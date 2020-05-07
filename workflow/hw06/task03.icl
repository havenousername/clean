module task03
import StdEnv

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



