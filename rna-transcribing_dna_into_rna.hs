-- http://rosalind.info/problems/rna/
main :: IO ()
main = do
	t <- getLine

	putStrLn $ map (\ti -> if ti == 'T' then 'U' else ti) t

