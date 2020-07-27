module Chap17Exercise where

l1 :: [Integer]
l1 = [1, 2, 3]

l2 :: [Integer]
l2 = [4, 5, 6]

p :: Maybe Integer
p = lookup 3 $ zip l1 l2

q :: Maybe Integer
q = lookup 2 $ zip l1 l2

summed :: Maybe Integer
summed = sum <$> ((,) <$> p <*> q)

----
data Pair a = Pair a a deriving (Show)

instance Functor (Pair) where
  fmap f (Pair a a') = Pair (f (a)) (f (a'))

instance Applicative (Pair) where
  pure x = Pair x x
  (Pair f g) <*> (Pair a a') = (Pair (f a) (g a'))

----
data Two a b = Two a b deriving (Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  (Two _ f) <*> (Two a' b') = Two a' (f b')

--List Applicative Exercise
data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Monoid (List a) where
  mempty = Nil

instance Semigroup (List a) where
  Nil <> Nil = Nil
  Nil <> (Cons x xs) = (Cons x xs)
  (Cons x xs) <> Nil = (Cons x xs)
  (Cons x xs) <> (Cons y ys) = (Cons x (xs <> (Cons y ys)))

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a (b)) = Cons (f a) (fmap f b)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> (Cons x xs) = Cons (f x) ((fmap f xs) <> (fs <*> (Cons x xs)))

