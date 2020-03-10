module rec_tail
import StdEnv

fac :: Real -> Real
fac n = facAux n 1.0

facAux :: Real Real -> Real
facAux 0.0 res = res  
facAux n res = facAux(n- 1.0) (res*n) 

Start = fac 100000000
// 3
// fac 3 = facAux 3 1
// facAux 3 1 = facAux(2)*(3*1)

