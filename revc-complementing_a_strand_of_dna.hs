-- http://rosalind.info/problems/revc/

dnaComplement 'C' = 'G'
dnaComplement 'G' = 'C'
dnaComplement 'A' = 'T'
dnaComplement 'T' = 'A'

main :: IO ()
main = do
	s <- getLine
	putStrLn $ map dnaComplement $ reverse s

