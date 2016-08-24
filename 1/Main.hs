module Main where

multiples :: Integer
multiples = sum [ x | x <- [1..999], x `mod` 5 == 0 || x `mod` 3 == 0 ] 

sumDivisibleBy :: Integer -> Integer -> Integer
sumDivisibleBy limit factor = factor * p * (p + 1) `div` 2
    where p = limit `div` factor

multiples' :: Integer
multiples' = sumDivisibleBy 999 3 + sumDivisibleBy 999 5 - sumDivisibleBy 999 15
