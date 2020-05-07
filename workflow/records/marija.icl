module marija
import StdEnv

favFood:: String [(String, String)] ->
favFood name food = [ x \\ (x,y) <- food | y==name]

Start = favFood "apple" [("Zuka", "apple"), ("Beka", "orange"), ("Emad", "pineapple"), ("Ahmed", "apple")]