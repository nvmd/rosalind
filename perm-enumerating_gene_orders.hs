-- http://rosalind.info/problems/perm/

import Control.Monad (liftM)

perm :: [a] -> [[a]]
perm (x:[]) = [[x]]
--perm (x:xs) = map (\p -> inject x 0 p) (perm xs)
perm (x:xs) = map (\p -> (concat $ map (\n -> inject x n p) [0..length p])) (perm xs)
perm []     = [[]]
--map (\p -> map (\n -> inject 42 n p) [0..2]) [[1,2],[34,5]]

inject :: a -> Int -> [a] -> [a]
inject e 0 xs     = e : xs
inject e n (x:xs) = x : inject e (n-1) xs
-- how to match on a negative n?
inject _ n _      = error "Wrong inject position number"
