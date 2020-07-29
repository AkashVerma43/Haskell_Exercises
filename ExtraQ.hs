module ExtraQ where

--data W w a = W (a,w);
--data R r a = R (r -> a);
--data C f g a = C (f (g a)) ;
--data P a = P (a -> Bool).
--data S = S (s -> (a, s))

--1
data W w a = W (a, w) deriving (Show)

instance Functor (W w) where
  fmap f (W (a, w)) = W (f a, w)

instance Monoid w => Applicative (W w) where
  pure a = W (a, mempty)
  W (f, w) <*> W (a, w') = W (f a, w <> w')

instance Monoid w => Monad (W w) where
  return = pure
  W (a, w) >>= f = let W (p, q) = f a in W (p, w <> q)

--trivial function
wowW :: Monoid m => Integer -> W m Integer
wowW a = W ((+ 1) a, mempty)

--2
data R r a = R (r -> a)

instance Functor (R r) where
  fmap f (R g) = R (f . g)

--Couldn't able to deduce what would be pure in applicative, same with below question
--3
data C f g a = C (f (g a)) deriving (Show)

instance (Functor f, Functor g) => Functor (C f g) where
  fmap h (C fga) = C ((fmap . fmap) h fga)

--5
--data S = S(s -> (a,s))
--not in scope : type variable 'a'
--not in scope : type variable 's'
