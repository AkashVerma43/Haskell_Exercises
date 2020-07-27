{-# LANGUAGE FlexibleInstances #-}

module Chap16Exercise where

--
data BoolAndSomethingElse a
  = False' a
  | True' a
  deriving (Show)

instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

--
data BoolAndMaybeSomethingElse a = Falsish | Truish a deriving (Show)

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

--
data Sum a b
  = First a
  | Second b
  deriving (Show)

instance Functor (Sum x) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

--
data Company a b c
  = DeepBlue a c
  | Something b
  deriving (Show)

instance Functor (Company e e') where
  fmap _ (Something b) = Something b
  fmap f (DeepBlue a c) = DeepBlue a (f c)

--
data More a b
  = L a b a
  | R b a b
  deriving (Show)

instance Functor (More x) where
  fmap f (L a b a') = L a (f b) a'
  fmap f (R b a b') = R (f b) a (f b')

--
data Quant a b
  = Finance
  | Desk a
  | Bloor b
  deriving (Show)

instance Functor (Quant x) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor (f b)

--
--data K a b = K a deriving Show
--instance Functor (K x) where
--  fmap _ (K a) = K a
--
newtype Flip f a b
  = Flip (f b a)
  deriving (Eq, Show)

newtype K a b = K a deriving (Show)

instance Functor (Flip K a) where
  fmap func (Flip (K b)) = Flip (K (func b))

--
data EvilGoateeConst a b = GoatyConst b deriving (Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)

--
data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Functor (List) where
  fmap _ Nil = Nil
  fmap func (Cons x xs) = Cons (func x) (fmap func xs)
