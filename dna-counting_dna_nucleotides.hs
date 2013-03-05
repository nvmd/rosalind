-- http://rosalind.info/problems/dna/?class=40
main :: IO ()
main = do
	s <- getLine
	putStrLn $ showRosalindAnswer
		 $ foldl (\(a,c,g,t) si ->
				case si of
				'A' -> (a+1,c,g,t)
				'C' -> (a,c+1,g,t)
				'G' -> (a,c,g+1,t)
				'T' -> (a,c,g,t+1)) (0,0,0,0) s
	where showRosalindAnswer (a,c,g,t) = (show a)
					   ++ " " ++ (show c)
					   ++ " " ++ (show g)
					   ++ " " ++ (show t)

