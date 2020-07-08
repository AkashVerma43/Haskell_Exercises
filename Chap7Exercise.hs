module Chap7Exercise where
--Let's write code
--1
tensDigit :: Integral a => a -> a
tensDigit x = d
 where xLast = x `div` 10
       d = xLast `mod` 10
--tens and hundred digit using divMod
td x = snd( (fst( (x) `divMod`(10))) `divMod` (10) )
hd x = snd( (fst( (x) `divMod`(100)) `divMod` (10)))
--
--2
foldbool :: a -> a -> Bool -> a
--one with case one with guard
--foldbool = error " Error : Need to implement foldbool"
--foldbool x y z =
--  case z of
--   True -> x
--   False -> y
foldbool x y z
 | z == True = x
 | z == False = y
--
--3
g::( [Char] -> Char ) -> ([Char] , c) -> (Char , c)
g f (a , c) = (f a , c )
f :: [Char] -> Char
f a = head a
--4
roundTrip :: (Show a , Read a) => a -> a
--roundTrip a = read (show a)
--5
roundTrip a = read.show $ a
main = do
 print ( roundTrip 4)
 print ( id 4 )
--6
roundTrip2 :: (Show a , Read b)=> a -> b
roundTrip2 a = read (show a)
