--GIORGOS SIDIROPOULOS


partition :: String -> [[String]]
partition s = delete s (makeParts (s)) --I delete the input String from my list  

makeParts :: String -> [[String]]
makeParts s 
	| length s == 1
		= [[[head s]]]
makeParts (h:t) = [(h:head y):(tail y) | y <- makeParts t] ++ [[h]:y | y <- makeParts t]
	
delete :: String -> [[String]] -> [[String]]
delete s (h:t) 
	| [s] == h
		= t
	|otherwise = h : delete s t
delete n [] = [] 

