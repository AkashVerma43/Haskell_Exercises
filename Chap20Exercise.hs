module Chap20Exercise where

--import Data.Foldable
import Data.Monoid

--Library Functions
--Implement in terms of foldMap or foldr
--
sum' :: (Foldable t, Num a) => t a -> a
sum' = getSum . foldMap Sum

--or
--sum' = foldr (+) 0

--
product' :: (Foldable t, Num a) => t a -> a
product' = getProduct . foldMap Product

--or
--product' = foldr (*) 1

--
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr ((||) . (x ==)) False

--or
--elem' x  = getAny.foldMap (Any.(x==))

--
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ l -> l + 1) 0

--
toList' :: (Foldable t) => t a -> [a]
toList' = foldr ((++) . (\x -> [x])) []

--
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap id

--Foldable instances of following types
--Just an Example from the chapter
data Optional a = Nada | Yep a deriving (Show)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

--1
data Constant a b = Constant a deriving (Show)

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

--2
data Two a b = Two a b deriving (Show)

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b

--  fold (Two _ b) = b

--3
data Three a b c = Three a b c deriving (Show)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

--4
data Three' a b = Three' a b b deriving (Show)

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = (f b) <> (f b')
