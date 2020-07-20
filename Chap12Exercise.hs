module Chap12Exercise where
--data Maybe a = NOthing | Just a
--data Either a b  = Left a | Right b
--String Processing
--1
notThe :: String -> Maybe String
notThe (x:xs) 
  | elem "the" (words(x:xs)) = Nothing
  | otherwise = Just (x:xs)

replaceThe :: String -> String
replaceThe (x:xs) = concat(replaceThe' (words(x:xs))("the")("a"))
replaceThe' :: [[Char]] -> [Char] ->[Char] -> [[Char]]
replaceThe' [] _ _ = []
replaceThe'(x:xs)(a)(b)
  |a == x = b : " " : replaceThe'(xs)(a)(b)
  |otherwise = x : " " : replaceThe'(xs)(a)(b)

--2
countTBBV :: String ->Integer
countTBBV [] = 0
countTBBV (str) = getCount(words(str))
getCount :: [[Char]] -> Integer
getCount [] = 0
getCount (x:xs)
  | x == "the" = if elem(head(head(xs)))("aeiouAEIOU") then (1 + getCount(xs)) else getCount(xs)
  | otherwise = getCount(xs)
--3
countVowels :: String ->Integer
countVowels [] = 0
countVowels (x:xs)
  |elem x "aeiouAEIOU" = 1+countVowels(xs)
  |otherwise = countVowels(xs)

--Validate the Word
newtype Word' = Word' String deriving(Eq,Show)
vowels = "aeiou"
mkWord :: String -> Maybe Word'
mkWord (str)
  | count > ( length(str) - (count)) = Nothing
  | otherwise = Just ( Word' str)
 where count = countVowel (str)(vowels)
countVowel :: String -> String ->Int
countVowel [] _ = 0
countVowel (x:xs)(vowels)
  | elem x vowels = 1 + countVowel (xs)(vowels)
  | otherwise = countVowel (xs)(vowels)

--Small Library For Maybe
--1
isJust :: Maybe a -> Bool
isJust (Nothing) = False
isJust (Just a) = True
--2
mayybee :: b -> ( a -> b) -> Maybe a -> b
mayybee x f m = 
  case m of
    Nothing -> x
    (Just a) -> f a
--3
fromMaybe :: a -> Maybe a -> a
fromMaybe a m =
  case m of
    Nothing -> a
    (Just a) -> a
--4
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x

maybeToList :: Maybe a -> [a]
maybeToList (Nothing) = []
maybeToList (Just a) = [a]
--5
catMaybe :: [Maybe a] -> [a]
catMaybe [] = []
catMaybe (x:xs) = (maybeToList x) ++ catMaybe (xs)
--6
flipMaybe ::Eq a => [Maybe a] -> Maybe [a]
flipMaybe (x:xs) = 
  case (elem (Nothing)(x:xs)) of
    True -> Nothing
    False -> Just (map(maybeTo)(x:xs))
maybeTo :: Maybe a -> a
maybeTo (Just a) = a

--Small Library For Either
--data Either a b = Left a | Right b --Convention error or invalid must be put in Left
--1
lefts' :: [Either a b] -> [a]
lefts' (x:xs) = foldr ((++).(\x -> case x of 
                                     Left x -> [x]
                                     Right x -> [] )) [] (x:xs)
--2
rights' :: [Either a b] -> [b]
rights' (x:xs) = foldr ((++).(\x -> case x of
                                      Left x -> []
                                      Right x -> [x])) [] (x:xs)
--3
partitionEithers' :: [Either a b] -> ([a],[b])
partitionEithers' (x:xs) = (lefts'(x:xs),rights'(x:xs))
--4
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f x =
  case x of 
    Left x -> Nothing
    Right x -> Just (f(x))
--5
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f g x =
  case x of
    (Left x) -> f$x
    (Right x) -> g$x

--Writing your own iterate and unfoldr
--1
myIterate :: (a -> a) -> a -> [a]
--myIterate f x = [f(x)] ++ myIterate (f) (f(x))
myIterate f x = [x] ++ myIterate f (f(x))
--2
myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f x = case f x of
                  Just (x , x') -> [x] ++ myUnfoldr (f)(x')
                  Nothing -> []

