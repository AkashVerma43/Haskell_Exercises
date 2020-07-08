module Chap6Exercise where
--does it type check?
--1
data Person = Person Bool deriving Show
printPerson :: Person -> IO()
printPerson person = putStrLn (show person)

--2 and 3.c
data Mood = Blah | Woot deriving ( Show ,Ord )
instance Eq Mood where
 ( == ) (Blah)
        (Blah) = True
 (==) (Woot)
      (Woot) = True
 (==) _
      _ = False
settleDown x = 
  if x == Woot
  then Blah
  else x

--4

type Subject = String
type Verb = String
type Object = String

data Sentence =
  Sentence Subject Verb Object deriving ( Eq , Show ) 
s1 = Sentence "dogs" "drool" "v"

-- Given datatype what can we do

data Rocks =
  Rocks String deriving (Eq, Ord, Show)
data Yeah =
  Yeah Bool deriving (Eq, Ord, Show)
data Papu =
  Papu Rocks Yeah deriving (Eq , Ord , Show)

--1.phew = Papu "chases" True
phew = Papu (Rocks "chases") (Yeah True)
--2. truth = Papu (Rocks "chomskydoz") (Yeah True)
truth = Papu (Rocks "chomskydoz") (Yeah True)
--3
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'
--4.
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

--Type-Kwon-Do Two: Electric Typealoo

--1. 
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk head a b = ( head a ) == b
--2. 
-- Hint: use some arithmetic operation to
-- combine values of type 'b'. Pick one.
arith :: Num b => (a -> b) -> Integer -> a -> b
arith arithSupport i x = ( arithSupport x ) + ( fromInteger i )
arithSupport :: Num b => a -> b
arithSupport = undefined
