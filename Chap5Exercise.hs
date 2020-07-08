module Chap5Exercise where
--Write type signature
--1
functionH :: [a] -> a
functionH (x : _) = x
--2
functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x > y) then True else False
--3
functionS ::( a , b ) -> b
functionS (x, y) = y
--Given types write function
--1
i :: a -> a
i x = x
--2
c :: a -> b -> a
c x y = x
--3
c'' :: b -> a -> b
c'' y x = y
--4
c' :: a -> b -> b
c' x y = y
--5
r :: [a] -> [a]
r xs = tail xs

--Fix it

--1 & 2
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"
sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"
sing = if (x > y) then fstString (x) else sndString (y)
  where x = "Singin" ;
        y = "Somewhere"

--3

main :: IO()
main =  do
  print (1 + 2)
  putStrLn "10"
  print (negate ( -1 ) )

--Type Kwon Do
--1
f :: Int -> String
f = undefined
g :: String -> Char
g = undefined
h :: Int -> Char
h x = g ( f x )
--2
data A
data B
data C
q :: A -> B
q = undefined
w :: B -> C
w = undefined
e :: A -> C
e x = w (q ( x ) )
--3 
data X
data Y
data Z
xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined
xform :: (X, Y) -> (Z, Z)
xform ( a , b ) = ( xz a , yz b )
--4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge nunge punge x = fst ( punge ( nunge x ) )
nunge :: x -> y
nunge = undefined
punge :: y -> ( w , z )
punge = undefined
