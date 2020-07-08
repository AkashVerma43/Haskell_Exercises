module Chap8Exercise where
--Recursion
--2
sumTill::(Eq a, Num a)=>a->a
sumTill 1 = 1
sumTill n = n + sumTill ( n - 1 )
--3
mul :: (Integral a) => a -> a -> a
mul _ 0 = 0
mul 0 _ = 0
mul a b = a + mul (a) ( b - 1 )
--Division with negative cases
type Numer = Integer
type Dino = Integer
type Quot = Integer
devide :: Numer -> Dino -> Quot
devide 0 _ = error "Error devide By Zero"
devide _ 0 = error "Error devide by Zero"
devide num dino 
 | num < 0 && dino < 0 = devideNow ( abs num ) ( abs dino) ( 0 )
 | num < 0 = negate( devideNow (abs num ) ( dino ) ( 0 ) )
 | dino < 0 = negate ( devideNow ( num ) ( abs dino ) ( 0 ))
 | otherwise = devideNow ( num ) ( dino ) ( 0 )
--
devideNow :: Numer -> Dino -> Quot -> Quot
devideNow n d q
 | n < d = q
 | otherwise = devideNow ( n - d ) ( d ) ( q + 1 )
--McCarthy 91 function
mc :: (Integral a) => a -> a
mc n
 | n > 100 = n - 10
 | otherwise = mc ( mc ( n + 11 ) )
--NUMBERS INTO WORDS
digitToWord :: Int -> String
--experimenting
--digitToWord n =
-- case ( n > 0 && n < 10 ) of
--  True -> show n
--  False -> error "Entered number is not digit"
digitToWord n
 | n == 1 = "One"
 | n == 2 = "Two"
 | n == 3 = "Three"
 | n == 4 = "Four"
 | n == 5 = "Five"
 | n == 6 = "Six"
 | n == 7 = "Seven"
 | n == 8 = "Eight"
 | n == 9 = "Nine"
 | otherwise = error "Entered entry was not digit"
isdigit a 
 | ( a < 10 && a > 0) = True
 | otherwise = False

--f :: Integral b => b -> [b]
f :: Int -> [Int]
f 0 = [ 0 ]
f n
 | isdigit(x) = (x) : ( (y) : [] )
 | otherwise = (f (x) ) ++ ( ( y ) : [] )
 where x = (div) (n) (10) ; y = (rem) (n) (10)
wordNumber :: Int -> [Char]
wordNumber n =concat (map digitToWord (f n ) )
