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

instance Applicative (R r) where
  pure a = R (\_ -> a)
  (R f) <*> (R g) = R (\x -> f x (g x))

instance Monad (R r) where
  return = pure
  (R g) >>= f = R $ (\r -> (let R h = f (g r) in (h r)))

--f :: a -> R r b
--g :: r -> a

--3
data C f g a = C (f (g a)) deriving (Show)

instance (Functor f, Functor g) => Functor (C f g) where
  fmap h (C fga) = C ((fmap . fmap) h fga)

instance (Applicative f, Applicative g) => Applicative (C f g) where
  pure x = C (pure (pure x))
  (C fgh) <*> (C fga) = C (((fmap) (<*>) fgh) <*> fga)

instance (Monad f, Monad g) => Monad (C f g) where
  return = pure
  C fga >>= f = do
    a <- C fga
    (f a)

--4
--data P a = P (a->Bool)

--5
data S s a = S (s -> (a, s))

instance Functor (S s) where
  fmap f (S g) = S $ (\s -> (let (a, x) = g s in (f a, x)))

instance Monoid s => Applicative (S s) where
  pure a = S (\_ -> (a, mempty))
  (S f) <*> (S g) =
    S $
      ( \s ->
          ( let (a, x) = g s
                (h, x') = f s
             in (fst (h, x') a, snd (h, x') <> x)
          )
      )

--g :: s -> (a , s)
--f :: s -> (a->b,s)
-- <*> :: S s (a->b) -> S s a -> S s b

instance Monoid s => Monad (S s) where
  return = pure
  (S g) >>= f =
    S $
      ( \s ->
          ( let (a, x) = g s
                S h = f a
                (b, x') = h s
             in (b, x <> x')
          )
      )

--f :: a -> S (s->(b,s))
