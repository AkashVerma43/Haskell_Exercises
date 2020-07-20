module Chap11Exercise where
import Data.Char
--As-patterns
-- @ READ ALOUD AS "AS"
doubleUp :: [a]  -> [a] 
doubleUp [] = [] 
doubleUp xs@(x:_) = x:xs
--
--1
isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] (y:ys) = True
--isSubsequenceOf [] [] = True
isSubsequenceOf _ [] = False
isSubsequenceOf (x:xs) (y:ys) 
  | x == y = isSubsequenceOf xs ys
  | otherwise = isSubsequenceOf (x:xs) ys
--with @
isSO ::(Eq a) => [a] -> [a] -> Bool
isSO [] ys = True
isSO _ [] = False
isSO l@(x:_) k@(y:_)
  | head(l) == head(k) = isSO (tail(l)) (tail(k))
  | otherwise = isSO (l) (tail(k))
--
--2
capitalizeWords :: String -> [(String,String)]
capitalizeWords [] = []
capitalizeWords (x:xs) = splitWords (words(x:xs))
splitWords :: [[Char]] -> [(String,String)]
splitWords [] = []
splitWords i@(y:ys) = [( head(i) , cap_it (head(i)))] ++ splitWords (tail(i))
cap_it :: String -> String
cap_it j@(x:xs) = toUpper(head(j)):tail(j)
--
--Language Exercise
--1
capitalizeWord :: String -> String
captalizeWord [] = []
capitalizeWord (x:xs) 
  |x /= ' ' = toUpper(x):xs
  |otherwise = x : capitalizeWord(xs)
--2
capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph (xs) = capitalizeWord (takeWhile (/= '.') xs) ++"."++ capitalizeParagraph(dropWhile (== '.') (dropWhile(/= '.')xs))
--PHONE
data DaPhone  = DaPhone [String]
data Digit = Char
data Presses = Int

keypad :: DaPhone
keypad = DaPhone ["1","abc2","def3","ghi4","jkl5","mno6","pqrs7","tuv8","wxyz9","^*"," 0",".,#"]

get_Phone :: DaPhone -> String -> [(Char,Int)]
get_Phone _ [] = []
get_Phone keypad (x:xs)
  | isUpper(x) = [('*',1)] ++ [get_tuple(keypad)(toLower(x))] ++ get_Phone(keypad)(xs)
  | otherwise = [get_tuple(keypad)(x)] ++ get_Phone(keypad)(xs)

get_tuple :: DaPhone -> Char -> (Char,Int)
get_tuple (DaPhone (y:ys)) x 
  | elem x y = (head(reverse(y)),get_count (x)(y)(1))
  | otherwise = get_tuple (DaPhone (ys)) (x) 

get_count :: Char -> String -> Int -> Int
get_count x (z:zs) c
  |x == z = c
  |otherwise = get_count(x)(zs)(c+1)

convo :: [String]
convo =  ["Wanna play 20 questions",
           "Ya",
           "U 1st haha",
           "Lol ok. Have u ever tasted alcohol lol",
           "Lol ya",
           "Wow ur cool haha. Ur turn",
           "Ok. Do u think I am pretty Lol",
           "Lol ya",
           "Haha thanks just making sure rofl ur turn"]
conversation = map (get_Phone keypad) convo

reverseTaps x
  |isUpper(x) =[('*',1)]++[get_tuple (keypad)(toLower(x))]
  |otherwise = [get_tuple keypad x]

fingerTaps :: [(Char,Int)] -> Int
fingerTaps [] = 0
fingerTaps (x:xs) = snd(x) + fingerTaps(xs)
--fingerTaps (get_Phone(keypad)("akash"))
--
--Hutton's Razor
data Expr
  = Lit Integer
  | Add Expr Expr

eval :: Expr -> Integer
eval (Lit x) = x
eval (Add (Lit a)(Lit b)) = eval(Lit a) + eval(Lit b)

printExpr :: Expr -> String
printExpr (Lit a) = show(a)
printExpr (Add a b) = printExpr(a) ++ "+" ++ printExpr(b)
