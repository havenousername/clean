module bill
import StdEnv

myBill :: Real Real -> Real
myBill cost gratuity = cost + (gratuity * cost)

Start = myBill 9001.00 0.08 //9721.08
