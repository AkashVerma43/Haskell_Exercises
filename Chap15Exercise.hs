module Chap15Exercise where

----
newtype Identity a = Identity a deriving (Show)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity a') = Identity (a <> a')

----
newtype BoolConj = BoolConj Bool deriving (Show)

instance Monoid (BoolConj) where
  mempty = (BoolConj False)

instance Semigroup (BoolConj) where
  (BoolConj b) <> (BoolConj b') = BoolConj (b || b')

----
newtype BoolDisj = BoolDisj Bool deriving (Show)

instance Monoid (BoolDisj) where
  mempty = (BoolDisj True)

instance Semigroup (BoolDisj) where
  (BoolDisj b) <> (BoolDisj b') = BoolDisj (b && b')

----
data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
  Nada <> Nada = Nada
  Nada <> (Only a) = Only a
  (Only a) <> Nada = Only a
  (Only a) <> (Only a') = Only (a <> a')

----
data Or a b
  = Fst a
  | Snd b
  deriving (Show)

instance Semigroup (Or a b) where
  (Snd b) <> _ = Snd b
  (Fst _) <> (Fst a') = Fst a'
  _ <> (Snd b) = Snd b

----
data Two a b = Two a b deriving (Show)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = (Two mempty mempty)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two a' b') = Two (a <> a') (b <> b')
