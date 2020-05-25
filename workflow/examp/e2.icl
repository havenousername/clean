module e2
import StdEnv

/* 1.
	Create a class MyMultDiv which has the operations *~ , /~ and has the neutral element as
	myOne. 
	So given two elements apply multiplication and division:
	*~ -> multiplication
	/~ -> division
	After that create an instace for Char.
	Hint: Operations in Char can be done on their ineteger representation and then convert back to string
	Be careful when you multiply and divide to stay in the range of 255 (you can use modulo)
*/

class MyMultDiv a
where
  (*~) infix 4 :: !a !a -> a 
  (/~) infix 4 :: !a !a -> a
  myOne :== toChar(1)
  // / (toInt ch2))  

instance MyMultDiv Char 
where
  (*~) ch1 ch2 
  | ch1 == myOne = ch2
  | ch2 == myOne = ch1 
   = toChar(((((toInt ch1) * (toInt ch2)) / 1000 + (((toInt ch1) * (toInt ch2)) / 100 ) rem 10) / 2 ) * 10 + (((toInt ch1) * (toInt ch2)) rem 10) / 4)
  (/~) ch1 ch2 = toChar((((toInt ch2) / 10 ) / 2 ) * 10 + (((toInt ch2) rem 10 )/ 2 ))

// Start = ('a' *~ 'b') //'G'
// Start = 'k' *~ myOne //'k'
// Start= 'z' /~ 'A' //''


::University={uniName::String,students::[Student],teachers::[Teacher]}
::Teacher={tname::String,subject::String}
::Student={studentName::String,age::Int,grades::{Int},favoriteTeacher::Teacher}
ELTE::University
ELTE={uniName="ELTE",students=[Marko,Nikola,Josh,Dame],teachers=[Mary,Peter,John]}
BME::University
BME={uniName="BMI",students=[Ana,Josh,Sofi,Nikola],teachers=[Viktor,John,Peter]}
EmptyUni::University
EmptyUni={uniName="Empty",students=[],teachers=[]}
Peter::Teacher
Peter={tname="Peter",subject="Functional"}
Viktor::Teacher
Viktor={tname="Viktor",subject="Math"}
Mary::Teacher
Mary={tname="Mary",subject="OOP"}
John::Teacher
John={tname="John",subject="Functional"}
Marko::Student
Marko={studentName="Marko",age=19,grades={4,4,4,5},favoriteTeacher= Mary}
Sofi::Student
Sofi={studentName="Sofi",age=22,grades={5,5,4,5,5},favoriteTeacher=John}
Dame::Student
Dame={studentName="Dame",age=21,grades={2,3,4,5},favoriteTeacher=Peter}
Ana::Student
Ana={studentName="Ana",age=18,grades={5,5,5,5},favoriteTeacher=Viktor}
Nikola::Student
Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},favoriteTeacher=Peter}
Nik::Student
Nik={studentName="Nik",age=20,grades={4,4,4,4,3},favoriteTeacher=Peter}
Nik2::Student
Nik2={studentName="Nik2",age=22,grades={4,4,4,4,5},favoriteTeacher=Peter}
Josh::Student
Josh={studentName="Josh",age=22,grades={4,5,5},favoriteTeacher=John}

/*2 Given a University, return an array of all the 
students or teachers names which are shorter than 6*/
//shorterThan6::University->{String}

getStudents:: University -> [String]
getStudents u=:{students = []} = []
getStudents u=:{students = [x:xs]} = [x.studentName] ++ getStudents { u&students = xs} 

getTeachers:: University -> [String]
getTeachers u=:{teachers = []} = []
getTeachers u=:{teachers = [x:xs]} = [x.tname] ++ getTeachers { u&teachers = xs} 

shorterThan6:: University -> {String}
shorterThan6 uni = {x \\ x <- ((getStudents uni) ++ (getTeachers uni)) | size x < 6} 

// Start = shorterThan6 ELTE
// Start = shorterThan6 BME

//Start=shorterThan6 BMI//{"Ana","Josh","Sofi","John","Peter"}
//Start=shorterThan6 ELTE//{"Marko","Josh","Dame","Mary","Peter","John"}
//Start=shorterThan6 EmptyUni//{}

// *3 Write a function which will take an array 
// of Universities and return the University with the highest overall gpa (the average of the average of each student)*/
//highestGpa::{University}->String

Universities:: {University}
Universities = {ELTE, BME, EmptyUni}

gpa:: {Int} -> Real
gpa gr = average
where
  l = [toReal(x) \\ x <-: gr]
  size = toReal (length l)
  average = (sum l) / size

studentGpa:: University -> [Real]
studentGpa u=:{students = [] } = []
studentGpa u=:{students = [x=:{grades}:xs] } = [gpa grades] ++ studentGpa {u&students=xs}

uniGpa:: University -> Real
uniGpa u=:{students} 
| length students == 0 = 0.0
= sum (studentGpa u) / toReal (length students) 

quickSortRecord:: [University] -> [University]
quickSortRecord [] = []
quickSortRecord [x: xs] = quickSortRecord [n \\ n <- xs | uniGpa n > uniGpa x ] ++ [x] ++ quickSortRecord [n \\ n <- xs | uniGpa n < uniGpa x ] 

highestGpa:: {University} -> String
highestGpa u 
| size u == 0 = abort "No universities given\n"
= name
where
 uL = [un\\ un <-: u]
 uS = hd(quickSortRecord uL)
 name = uS.uniName


// Start = highestGpa Universities
// Start=highestGpa {ELTE,BME} //"BMI"
// Start=highestGpa {EmptyUni,EmptyUni}//"Empty"
// Start=highestGpa {ELTE} //"ELTE"
// Start=highestGpa {}//"No universities given"


/*4	Create a toString instance for Student such that for given student ex. Nikola={studentName="Nikola",age=19,grades={4,4,4,4,2},
favoriteTeacher=Peter} it gives "Nikola 3.6 Peter" where 3.6 is the student's gpa and Peter is the student's favorite teacher's name*/
//instance toString Student


instance toString Student
where
  toString {studentName = n, grades = g, favoriteTeacher={tname=fT} } = n +++ " " +++ toString(gpa g) +++ " " +++ fT


// Start = toString Nikola
// Start=toString Marko//"Marko  4.25  Mary"
// Start=toString Nik//"Nik  3.8  Peter"   
// Start=toString Dame//"Dame  3.5  Peter"


/* 5
A good person is the person who never lies, so let's test this quote, we have list of people, each person has name
and list of the names of the people that he lies to, 
your task is to get the people who can say the truth to all the people in the given list 
list can be empty if all the people did lie.
Example : goodPeople [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel"]},{fake_name = "Lucifier", peopleToLie = ["Rafaat Ismail"]}
Output : [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel"]}] because Adel is not on the given list.
note : ofcourse we will consider fake names for this expirement so all the names here are fictional.
*/
::Person = {fake_name :: String, peopleToLie :: [String]}


gPerson:: [Person]
gPerson = [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel","Maggi"]},{fake_name = "Lucifier", peopleToLie = ["Adel","Rafaat Ismail"]},{fake_name = "elkenona", peopleToLie = ["Adel","Lucifier"]}]

// takeFakeNames:: [Person] -> [String]
// takeFakeNames p = map (\{fake_name = n} = n) p

peopleToLie:: Person -> [String]
peopleToLie {peopleToLie = pL} = pL 

goodPeople:: [Person] -> [Person]
goodPeople person=:[x:xs] = filterGoodPeople
where 
  takeFakeNames = map (\{fake_name = n} = n) person
  filterGoodPeople = filter (\x = and[ (pL <> tF) \\ pL <- (peopleToLie x) , tF <- takeFakeNames ]) person


// Start = takePeopleToLie {fake_name = "Rafaat Ismail", peopleToLie = ["Adel","Maggi"]}
// Start = goodPeople gPerson

// goodPeople :: [Person] -> [Person]
  


// Start = goodPeople [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel","Maggi"]},{fake_name = "Lucifier", peopleToLie = ["Adel","Rafaat Ismail"]},{fake_name = "elkenona", peopleToLie = ["Adel","Lucifier"]}] // [{fake_name = "Rafaat Ismail", peopleToLie = ["Adel","Maggi"]}]
// Start = goodPeople [{fake_name = "Alaa Abdelazim", peopleToLie = ["Brnadt","Shelby"]},{fake_name = "Bartaleaa", peopleToLie = ["Alaa Abdelazim","Shelby"]},{fake_name = "Shelby", peopleToLie = ["Bartaleaa"]}] // []


//6. Given two arrays, return new array such that i-th element of it is maximum of i-th element of first and second arrays.
// So for example, when we calculate 5th element of result array, we look at 5th element of first and 5th element of second arrays
// And choose maximum of the two.
// You can assume that arrays have same length. 


maxOfTwo :: {Int} {Int} -> {Int}
maxOfTwo arr1 arr2 = {max a1 a2 \\ a1 <-: arr1 &  a2 <-: arr2 }


// Start = maxOfTwo {1} {5} // {5}
// Start = maxOfTwo {1,5,4} {2,3,6} // {2,5,6}
// Start = maxOfTwo {1,2,3,4,5} {1,2,3,4,5} // {1,2,3,4,5}


//7. You are given array of integers.
// Your function should return true if each value appears at least twice in the array, and it should return false
// if any element is distinct.

count:: [Int] -> Int
count [] = 0
count [x:xs] = 1 + count xs 

countDup:: [Int] -> [Int]
countDup [] =[]
countDup [x:xs] = [count (takeWhile (\y = y == x) xs) + 1] ++ countDup (dropWhile (\y = y == x) xs)


f7 :: {Int} -> Bool
f7 arr 
| or[x > 2\\ x <- count] = True
= False
where
  l = [el\\ el <-: arr]
  s = sort l 
  count = countDup s 



// Start = f7 {1,2,3,1,3,2,2,2} // True
// Start = f7 {1,2,3,4,3,2,1} // False
// Start = f7 {1,1,1,3,3,4,3,2,4,2} // True


//8.Array is monotonic if it is either monotone increasing or monotone decreasing
// A is monotone increasing if for all i<=j, A[i]<=A[j]
// A is monotone decreasing if for all i<=j, A[i]>=A[j]
// Given array, your task is to decide if it is monotonic.

allBigger::[Int] -> [Bool]
allBigger [] = []
allBigger [l1] = [True]
allBigger [l1,l2:ls] 
| l1 >= l2 = [True] ++ allBigger [l2:ls] 
= [False] ++ allBigger [l2:ls] 

allSmaller::[Int] -> [Bool]
allSmaller [] = []
allSmaller [l1] = [True]
allSmaller [l1,l2:ls] 
| l1 <= l2 = [True] ++ allSmaller [l2:ls] 
= [False] ++ allSmaller [l2:ls] 

isMonotonic :: {Int} -> Bool
isMonotonic arr 
| and[True == l \\ l <- incMono] = True 
| and[True == l \\ l <- incMono1] = True 
= False
where 
  list = [el \\ el <-: arr]
  incMono = allBigger list
  incMono1 = allSmaller list

// Start = isMonotonic {6,5,4,4} // True
// Start = isMonotonic {1,3,2} // False
// Start = isMonotonic {1,2,4,5} // True
// Start = isMonotonic {1,1,1} // True



/* 9
Please write a function that, given a Tree and a predicate,
will find nodes that do not return True for the predicate
and will remove those nodes and their subtrees.
Note: The expected return results are listed below with an equality
for your convenience, so that you do not have to manually check your result.
If your result is correct, the Start statement should return a True.
*/




:: Tree a = Node a (Tree a) (Tree a) | Leaf

instance == (Tree a) | == a
where
    == Leaf Leaf = True
    == (Node x1 l1 r1) (Node x2 l2 r2) = and[x1==x2, l1==l2, r1==r2]
    == _ _ = False

specialTree :: (Tree Int)
specialTree = Node 10 (Node 4 (Node 1 (Node 0 Leaf Leaf)(Node 2 Leaf Leaf))(Node 5 Leaf (Node 6 Leaf Leaf)))(Node 15 (Node 12 (Node 11 Leaf Leaf)(Node 13 Leaf Leaf))(Node 17 (Node 16 Leaf Leaf)(Node 19 (Node 18 Leaf Leaf)(Node 20 Leaf Leaf))))

notPrime :: Int -> Bool
notPrime x
| x <= 1 = True
= not(isEmpty[n\\n<-[2..(x-1)]|x rem n == 0])

extractNode:: (Tree Int) -> Int 
extractNode Leaf = 0
extractNode (Node x l r) = x

goL:: (Tree a) -> (Tree a)
goL (Node x l r) = l

goR:: (Tree a) -> (Tree a)
goR (Node x l r) = r

pruneTree :: (Tree a) (a -> Bool) -> (Tree a)
pruneTree Leaf _ = Leaf
pruneTree (Node x l r) fq 
| (fq x) == True =  Node x (pruneTree l fq) (pruneTree r fq)
= Leaf

// Start = pruneTree specialTree isEven == (Node 10 (Node 4 Leaf Leaf) Leaf) //True
// Start = pruneTree specialTree ((<)7) == (Node 10 Leaf (Node 15 (Node 12 (Node 11 Leaf Leaf) (Node 13 Leaf Leaf)) (Node 17 (Node 16 Leaf Leaf) (Node 19 (Node 18 Leaf Leaf) (Node 20 Leaf Leaf))))) //True
// Start = pruneTree specialTree notPrime == (Node 10 (Node 4 (Node 1 (Node 0 Leaf Leaf) Leaf) Leaf) (Node 15 (Node 12 Leaf Leaf) Leaf)) //True


/*10
Given a tree and an integer. Find all the nodes that equal to the integer and give the sum 
of their direct children.(Leaf count as 0).
*/
f10::(Tree Int) Int->Int
f10 Leaf y = 0
f10 (Node x l r) y 
| x == y = (extractNode l + extractNode r) + (f10 l y) + (f10 r y)
= (f10 l y) + (f10 r y)

// Start = f10 (Node 3 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf)) 3 //2
// Start = f10 (Node 3 (Node 1 Leaf Leaf) (Node 1 Leaf Leaf)) 3 //2
// Start = f10 (Node 1 (Node 0 Leaf Leaf)(Node 2 Leaf Leaf)) 1 //2
// Start = f10 (Node 2 (Node 1 Leaf Leaf)(Node 2 (Node 3 Leaf Leaf) (Node 1 Leaf Leaf))) 2 //7
// Start = f10 (Node 2 (Node 1 Leaf Leaf)(Node 2 Leaf (Node 1 Leaf Leaf))) 2 //4








// Start = uniGpa BME