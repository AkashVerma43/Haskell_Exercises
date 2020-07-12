module Chap9Exercise where
import Data.Char
--Data.Char
--2
allUpper::[Char]->[Char]
allUpper [] = []
allUpper xs = filter isUpper xs
--3
capFirst :: [Char]->[Char]
capFirst [] = []
capFirst (x:xs)
   |isLower x = (toUpper x) : xs
   |otherwise = x:xs
--4
capAll :: [Char] -> [Char]
capAll [] = []
capAll (x:xs)
   |isLower x = ( toUpper x ) : capAll xs
   |otherwise = x:capAll xs
--5
capHead :: [Char] -> Char
capHead [] = ' '
--capHead xs = head $ capFirst xs
--6
--capHead xs = head.capFirst $ xs
capHead xs = head $ capFirst $ xs
--Writing your own standard functions
--1
myOr::[Bool]->Bool
myOr [] = False
myOr (x:xs) = if x == True then True else myOr xs
--2
myAny :: ( a -> Bool ) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = if f $ x then True else myAny f (xs)
--3
myElem :: Eq a => a -> [a] -> Bool 
myElem _ [] = False
myElem a (x:xs) = if a == x then True else myElem a xs
--4
myReverse :: [a]->[a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]
--5
squish:: [[a]] -> [a]
squish [] = []
squish (x:xs) = x++squish xs
--6
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs
--7
squishAgain :: [[a]]->[a]
squishAgain [] = []
squishAgain (x:xs) = squishMap g x ++ squishAgain xs
g :: a -> [a]
g x = [x]
--8
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy compare (x:xs)
   | length(xs) == 0 = x
   | compare (x) (head(xs)) == GT = if compare x y ==GT then x else y
   | otherwise = y
   where y = myMaximumBy compare xs
--9
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy compare (x:xs)
   | length(xs) == 0 = x
   | compare (x) (head(xs)) == LT = if compare x y ==LT then x else y
   | otherwise = y
   where y = myMinimumBy compare xs
--
myMax :: (Ord a) => [a] -> a
myMax xs = myMaximumBy compare xs
myMin :: (Ord a) => [a] -> a
myMin xs = myMinimumBy compare xs
