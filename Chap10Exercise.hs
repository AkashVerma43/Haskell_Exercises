module Chap10Exercise where
--Warm-up Review
--1
comb :: [a] -> [b] -> [a] -> [(a,b,a)]
comb [] _ _ = []
--comb (x:xs)(y:ys)(z:zs) = g x (y:ys)(z:zs) ++ comb(xs)(y:ys)(z:zs)
--comb (x:xs)(y:ys)(z:zs)
--  | x == 'p' = g x (y:ys)(z:zs)
--  | otherwise = comb (xs)(y:ys)(z:zs)
comb (x:xs)(y:ys)(z:zs) = g x(y:ys)(z:zs)
g :: a -> [b] -> [a] -> [(a,b,a)]
g _ [] _ = []
g x (y:ys)(z:zs) = h x y (z:zs) ++ g x (ys) (z:zs)
h :: a -> b -> [a] -> [(a,b,a)]
h _ _ [] = []
h x y (z:zs) = [(x,y,z)] ++ h x y (zs)

--2
seekritFunc ::String -> Int
seekritFunc x =
  div (sum (map length (words x)))
       (length (words x))


--Rewriting functions using folds

--fold for myAnd and point free folding function
myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True
--1
myOr :: [Bool] -> Bool
myOr = foldr (||) False
--2
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||).f) False
--3
myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr ((||).((==)a)) False
--4
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) [] 
--5
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:).f) []
--6
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr (\x -> if f x == True then ([x]++) else ([]++)) []
--7
squish :: [[a]] -> [a]
squish = foldr (++) []
--8
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++).f) []
--9
squishAgain :: [[a]] -> [a]
squishAgain  = foldr ((++).(squishMap (\x -> [x]))) []

--Took reference from internet for below questions
--Didn't knew about foldr1 before
--10
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy cmp = foldr1 (\x z -> case cmp x z of 
  GT -> x
  _  -> z)
--11
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy cmp = foldr1 (\x z -> case cmp x z of
  LT -> x
  _  -> z)
