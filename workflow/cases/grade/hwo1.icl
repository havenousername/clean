module hwo1
import StdEnv

myGrade :: Real -> Int
myGrade input
| input < 0.0 = -1
| input < 51.0 = 1
| input < 61.0 = 2
| input < 71.0 = 3
| input < 86.0 = 4
| input <= 100.0 = 5
= -1 

Start = myGrade -3.42
