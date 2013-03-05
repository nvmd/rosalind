-- http://rosalind.info/problems/hamm/?class=40

main :: IO ()
main = do
	s <- getLine
	t <- getLine

	putStrLn $ show
		 $ foldl (\diff (si,ti) ->
			  case si /= ti of
			  	True -> diff + 1
				_    -> diff) 0 $ zip s t

