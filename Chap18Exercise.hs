module Chap18Exercise where

data Somename b a
  = Left' a
  | Right' b
  deriving (Show)

instance Functor (Somename b) where
  fmap _ (Right' b) = (Right' b)
  fmap func (Left' a) = Left' (func a)

instance Applicative (Somename b) where
  pure = Left'
  (Right' b) <*> _ = Right' b
  _ <*> (Right' b) = Right' b
  (Left' func) <*> l = fmap func l

instance Monad (Somename b) where
  return = pure
  Right' x >>= _ = Right' x
  Left' x >>= func = func x

--Simple trivial func of type Integer -> Somename
--evaluate based on x being even or odd
notImpFunc :: Integer -> Somename Integer Integer
notImpFunc x
  | even x = Right' x
  | otherwise = Left' (x + 1)

---
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap func (Identity a) = Identity (func a)

instance Applicative Identity where
  pure = Identity
  Identity func <*> j = fmap func j

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

---
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
  fmap func (Cons x xs) = Cons (func x) (fmap func xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> (Cons x xs) = Cons (f x) ((fmap f xs) <> (fs <*> (Cons x xs)))

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  (Cons x xs) >>= f = (f x) <> (xs >>= f)

--just a trivial function
incrementElemList :: Integer -> List Integer
incrementElemList x = (Cons (x + 1) Nil)
