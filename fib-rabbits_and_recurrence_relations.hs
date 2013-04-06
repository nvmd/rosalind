-- http://rosalind.info/problems/fib/

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fib' :: Integer -> Integer
fib' n = rec n 1

rec :: Integer -> Integer -> Integer
rec 0 _ = 0
rec 1 _ = 1
rec n k = (rec (n - 1) k) + k * (rec (n - 2) k)

main :: IO ()
main = do
	s <- getLine
	let args = words s
	let n = read $ args !! 0 :: Integer
	let k = read $ args !! 1 :: Integer
	
	putStrLn $ show $ rec n k

