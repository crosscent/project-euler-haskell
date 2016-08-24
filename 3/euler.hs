-- Starting from the smallest prime number, with x being the number being
-- factorialized
-- Divide x the prime number until it can't be evenly divided anymore, and this
-- will ensure that the next number that can evenly divide what's left over is
-- a prime
reduce :: Integer -> Integer -> Integer
reduce x y
    | x `mod` y == 0    = reduce (x `div` y) y
    | otherwise         = x


lastFactor' :: Integer -> Integer -> Integer
lastFactor' x y 
    | x == 1    = y
    | x `mod` y == 0    = lastFactor' (reduce x y) y
    | otherwise = lastFactor' x (y + 1)

lastFactor :: Integer -> Integer
lastFactor x = lastFactor' x 2
