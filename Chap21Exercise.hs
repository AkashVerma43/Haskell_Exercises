module Chap21Exercise where

--import Data.Traversable
--import Data.Monoid
--Traversable
--class (Functor t , Foldable t) => Traversable t where
--  traverse :: Applicative f => (a-> f b) -> t a -> f (t b)
--  traverse f = sequenceA . fmap f
--  sequenceA :: Applicative f => t (f a) -> f (t a)
--  sequenceA = traverse id

--instance for either (example from book)
data E a b = L a | R b deriving (Eq, Show, Ord)

instance Functor (E e) where
  fmap f (R b) = R (f b)
  fmap _ (L e) = L e

instance Applicative (E e) where
  pure = R
  L e <*> _ = L e
  R f <*> r = fmap f r

instance Foldable (E e) where
  --  foldmap ::(Monoid m) => (a -> m) -> t a -> m
  foldMap _ (L _) = mempty
  foldMap f (R b) = f b

instance Traversable (E e) where
  traverse _ (L e) = pure (L e)
  traverse f (R b) = fmap R (f b)

--Exercise
--Traversable Instances

--Identity
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Foldable Identity where
  foldMap f (Identity a) = f a

instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

--Constant
newtype Constant a b = Constant {getConstant :: a}

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Foldable (Constant a) where
  foldMap _ (Constant _) = mempty

instance Traversable (Constant a) where
  traverse _ (Constant a) = pure (Constant a)

--Maybe
data Optional a = Nada | Yep a

instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)

instance Foldable Optional where
  foldMap _ Nada = mempty
  foldMap f (Yep a) = f a

instance Traversable Optional where
  traverse _ Nada = pure $ Nada
  traverse f (Yep a) = Yep <$> (f a)

--List
data List a = Nil | Cons a (List a)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = (f x) <> (foldMap f xs)

instance Traversable List where
  traverse _ Nil = pure $ Nil
  traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)

--Three
data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
  traverse f (Three a b c) = (Three a b) <$> (f c)

--Three'
data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a b b') = Three' a (f b) (f b')

instance Foldable (Three' a) where
  foldMap f (Three' _ b b') = (f b) <> (f b')

instance Traversable (Three' a) where
  traverse f (Three' a b b') = (Three' a) <$> (f b) <*> (f b')

--Traversable Instance for Tree
data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap _ Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
  --foldMap :: Monoid m => (a -> m) -> t a -> m
  foldMap _ Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node left a right) = (foldMap f left) <> (f a) <> (foldMap f right)

instance Traversable Tree where
  traverse _ Empty = pure $ Empty
  traverse f (Leaf a) = Leaf <$> (f a)
  traverse f (Node left a right) = Node <$> (traverse f left) <*> (f a) <*> (traverse f right)
