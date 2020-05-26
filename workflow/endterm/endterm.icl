module endterm
import StdEnv


::Position=Goalkeeper|Defender|Midfielder|Striker
::Player={name::String,team::String,position::Position,goalsScored::[Int]}
 
VanPersie::Player
VanPersie={name="Robin",team="Manchester United",position=Striker,goalsScored=[20,25,28,30]}
 
Rooney::Player
Rooney={name="Wayne",team="Manchester United",position=Striker,goalsScored=[25,20,20,18,22,24]}
 
Ibrahimovic::Player
Ibrahimovic={name="Zlatan",team="Inter",position=Striker,goalsScored=[19,17,19,16,17,18]}
 
Kroos::Player
Kroos={name="Tony",team="Real Madrid",position=Midfielder,goalsScored=[6,13,12,15]}
 
VanDerSar::Player
VanDerSar={name="Edwin",team="Manchester United",position=Goalkeeper,goalsScored=[0,0,0,1,0,2]}
 
Vidic::Player
Vidic={name="Nemanja",team="Manchester United",position=Defender,goalsScored=[8,5,8,6,4,11]}
 
Ferdinand::Player
Ferdinand={name="Rio",team="Manchester United",position=Defender,goalsScored=[5,4,3,5]}
 
::Tree a=Node a (Tree a) (Tree a)|Leaf
 
FirstTree= Node VanPersie (Node VanDerSar (Node Rooney (Node Ibrahimovic Leaf Leaf) Leaf) (Node VanPersie Leaf Leaf)) (Node Vidic (Node Vidic Leaf Leaf) (Node Kroos Leaf Leaf))
SecondTree= Node Vidic (Node Kroos Leaf (Node Rooney Leaf Leaf)) Leaf
ThirdTree= Node Kroos (Node Ibrahimovic Leaf (Node Rooney Leaf Leaf)) (Node VanDerSar Leaf (Node Ferdinand (Node VanPersie Leaf Leaf) Leaf))


 
/*Given a structure Player. 
Each Player has 
 name-String,
 team-String,
 position-Position,
 goalsScored-[Int] 
fields.*/
 
/*For grade 2:
Given a list of Players, return a list of triples like:(player name,team,total number of goals) 
for the players having total goals scored less than 70
*/



triplesPlayers::[Player]->[(String,String,Int)]
triplesPlayers [] = []
triplesPlayers [x=:{name=n, team=t, goalsScored=gS}: xs] 
|  sum gS < 70 = [(n, t, sum gS): triplesPlayers xs]  
= triplesPlayers xs


 
// Start=triplesPlayers [VanPersie,Rooney,Ibrahimovic,Kroos,VanDerSar] //[("Tony","Real Madrid",46),("Edwin","Manchester United",3)]
// Start=triplesPlayers [VanPersie,Rooney]//[]
// Start=[VanDerSar,Vidic]//[(Player "Edwin" "Manchester United" "Goalkeeper" [0,0,0,1,0,2]),(Player "Nemanja" "Manchester United" "Defender" [8,5,8,6,4,11])]
 
/*For grade 3:
Given an array of players, give the best striker who doesn't play for "Real Madrid"
A best striker is the player who has position striker and highest aveage of goals scored
IMPORTANT:The average value must be a real number
*/

// GoalsAverage:: [Int] ->  Real 
// GoalsAverage l = toReal (sum l) / toReal (length l)

// takeBiggest:: [Player] Int -> Player
// takeBiggest [x=:{goalsScored=gS}: xs] y 
// | GoalsAverage gS > toReal y = takeBiggest xs (GoalsAverage gS)


// bestStrikerNotMadrid::{Player}->Player
// bestStrikerNotMadrid arr = [l \\ l <-: arr]




 
//Start=bestStrikerNotMadrid {VanPersie,Rooney,Ibrahimovic,Kroos,VanDerSar}//(Player "Robin" "Manchester United" Striker [20,25,28,30])
//Start=bestStrikerNotMadrid [Kroos,Vidic,Rooney]//(Player "Wayne" "Manchester United" Striker [25,20,20,18,22,24])
 
/*For grade 4:
Given a list of players, write a function which will count how many players are equal to the first player of the list.
Keep in mind thatt wo players are equal if they have the same name, play for the same team on the same position
If the list is empty return 0
*/
 
eQ:: [Player] String String Position -> Int
eQ [] _ _ _= 1
eQ [x=:{name=n, team = t, position=p}:xs] y z p1
| n == y && z == t && p == p1  = 1 + eQ xs y z p1
= eQ xs y z p1

equalPlayer::[Player]->Int
equalPlayer [x=:{name=n, team = t, position= p}:xs] = eQ xs n t p

instance == Position
where
  (==) Goalkeeper Goalkeeper = True
  (==)Defender Defender = True
  (==) Midfielder Midfielder = True
  (==) Striker  Striker = True
 
// Start=equalPlayer [VanPersie,VanPersie,VanPersie]//3
// Start=equalPlayer [VanPersie,VanDerSar,Vidic,Vidic]//1
// Start=equalPlayer [Vidic,VanPersie,VanDerSar,Vidic,Vidic]//3
//Start=equalPlayer []//0
 
/*For grade 5:
Given a Tree of Players. Write a function which will give the least total number of goals scored by a Player from the Tree.
*/ 
 
mostGoals::(Tree Player)->[Int]
mostGoals Leaf = []
mostGoals (Node {goalsScored} l r) = [sum (goalsScored)] ++ (mostGoals l) ++ (mostGoals r)
 
mS:: (Tree Player) -> Int
mS tree = minList (mostGoals tree)

// Start=mS FirstTree//3
// Start=mS SecondTree//42
// Start=mS ThirdTree//3