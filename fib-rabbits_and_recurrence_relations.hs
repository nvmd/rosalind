-- http://rosalind.info/problems/fib/

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- infinite fibonacci sequence (A Gentle Introduction to Haskell, Version 98)
-- fib          = 1 : 1 : [ a+b | (a,b) <- zip fib (tail fib) ]
-- fib@(1:tfib) = 1 : 1 : [ a+b | (a,b) <- zip fib tfib ]

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

---- http://www.haskell.org/haskellwiki/Let_vs._Where#Lambda_Lifting
---- http://www.haskell.org/pipermail/haskell-cafe/2010-October/084538.html
--fib = (map fib' [0 ..] !!)
--    where
--      fib' 0 = 0
--      fib' 1 = 1
--      fib' n = fib (n - 1) + fib (n - 2)

--fib x = map fib' [0 ..] !! x
--    where
--      fib' 0 = 0
--      fib' 1 = 1
--      fib' n = fib (n - 1) + fib (n - 2)

---- translates into
-- More efficient
-- fib' can be moved to the top level by a compiler
--fib =
--    let fib' 0 = 0
--        fib' 1 = 1
--        fib' n = fib (n - 1) + fib (n - 2)
--    in  (map fib' [0 ..] !!)

-- Now we create a new fib' for each invocation of fib.  Not efficient at
-- all!  (Much *less* efficient the the recursive fib).
-- fib' is (re-)defined for every argument x, thus it cannot be floated out.
--fib x =
--    let fib' 0 = 0
--        fib' 1 = 1
--        fib' n = fib (n - 1) + fib (n - 2)
--    in  map fib' [0 ..] !! x

