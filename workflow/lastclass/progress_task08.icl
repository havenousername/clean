module progress_task08
import StdEnv

:: Q = { nom::Int
		, denom :: Int }
mkQ n d= {nom=n,denom=d}

/*
	Create an instance for the '==' operation on Rational numbers
*/

instance == Q
where
  (==) {nom = n1, denom = d1} {nom = n2, denom = d2} = (n1 * d2) == (d1 * n2)


// Start = mkQ 1 2 == mkQ 3 12 //False
// Start = mkQ 1 2 == mkQ 1 2 //True
// Start = mkQ 1 2 == mkQ 4 8 //True
