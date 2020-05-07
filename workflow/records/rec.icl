module rec
import StdEnv


//  In tuples order is important!
myBirthday :: (Int, String, Int)
myBirthday = (2000, "April", 01)



// Start = snd3 myBirthday

:: Date = {
            year:: Int,
            month:: Month,
            day:: Int
          }

evanBDay = {year=2000, month=Apr, day=1 } 
// americanBDay = {month=Apr, year=1982, day=12}

::Person = {
              name:: String,
              favouriteColors:: [String],
              birthday:: Date,
              isSingle:: Bool
            }



Evan:: Person
Evan = {birthday = evanBDay, favouriteColors = ["Blue", "Red", "White"], name="Evan", isSingle = True}
Even:: Person
Even = {birthday = evanBDay, favouriteColors = ["Blue", "Red", "White"], name="Evan", isSingle = True}


::Point a = {x::a, y::a, z::a}


p1:: (Point Int)
p1 = {x=1, y=2, z=3}


p2:: (Point Real)
p2 = {x=1.111, y=2.23, z=3.132}

p3:: (Point Bool)
p3 = {x=False, y=False, z=True}


// ::WeildRecond a b c e = {x::(a,b), y:: [c], z::c, e::Bool}

// wr = {x=("Evan", 41.234), y=[1,34,341,23], z="Hello", r=True}

// Start = wr


// Start = p3
// Start = Evan
// Start = Evan.name
// Start = evanBDay // (Date 2000 "April" 1)
// Start = americanBDay  //(Date 1982 "May" 12)

// Start = evanBDay.month //"April"
// Start = {year=2000} // compile error


getBirthMonth:: Person -> Month
getBirthMonth p=:{birthday = {month = m}} 
| p.isSingle = m
= Jan
// getBirthMonth {name=n, favouriteColors= f, birthday=b, isSingle=s} = b.month
// getBirthMonth p = p.birthday.month 

updateSingleStatus:: Person -> Person
updateSingleStatus p=:{isSingle = s} = {p & isSingle = not s}
// updateSingleStatus p = {p & isSingle = not p.isSingle}
// updateSingleStatus p = {isSingle = not p.isSingle, name = p.name, favouriteColors = p.favouriteColors, birthday = p.birthday}



// Start = getBirthMonth (updateSingleStatus Evan)

updateColor:: Person -> Person
updateColor p=:{isSingle = s, favouriteColors = f}
| s = {p & isSingle = not s, favouriteColors = f ++ ["Violet"]}
= {p & isSingle = not s, favouriteColors = []}
// | p.isSingle = {p & isSingle = not p.isSingle, favouriteColors = p.favouriteColors ++ ["Violet"]}
// = {p & isSingle = not p.isSingle, favouriteColors = []}


instance < Person
where
  (<) {birthday={year=y1}} {birthday={year=y2}} = y1 < y2

instance == Month 
where 
  (==) Jan Jan = True
  (==) Feb Feb = True
  (==) Mar Mar = True
  (==) Apr Apr = True
  (==) _ _ = False
instance == Date where (==) {day=d1, year=y1, month=m1} {day=d2, year=y2, month=m2}  = d1 == d2 && y1==y2 && m1==m2

instance == Person
where
  // (==) p1 p2 = p1.name == p2.name && p1.isSingle == p2.isSingle
  (==) {name=n1,birthday=b1} {name=n2,birthday=b2} = n1==n2 && b1 == b2


// Start = updateColor(updateSingleStatus Evan) 


// Start = Evan == Even

// listPeople::[Int]
listPeople = [Evan, Even]

Start = sort listPeople

::Month = Jan | Feb | Mar | Apr




