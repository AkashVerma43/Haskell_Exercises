module Chap9Erxercise_Cipher where
import Data.Char

data Direction = L | R deriving Eq

cipher :: Direction -> Int -> [Char] -> [Char]
cipher _ _ [] = []
cipher dir num msg 
   | dir == L = leftCipher shift msg
   | dir == R = rightCipher shift msg
   where shift = num `mod` 26

rightCipher :: Int -> [Char] -> [Char]
rightCipher _ [] = []
rightCipher shift (x:xs) 
   | ord x + shift > 122 = chr (96 + (ord x + shift - 122) ) : rightCipher shift xs
   | otherwise = chr (ord x + shift) : rightCipher shift xs

leftCipher :: Int -> [Char] -> [Char]
leftCipher _ [] = []
leftCipher shift (x:xs)
   | ord x - shift < 97 = chr (123 - ( 97 - (ord x - shift) ) ) : leftCipher shift xs
   | otherwise = chr (ord x - shift) : leftCipher shift xs

--This code is In Improvement
