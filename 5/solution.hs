primes :: [Integer]
primes = [ x | x <- [2..], isPrime x]
    where isPrime x = and $ map(\y -> x `mod` y /= 0) [2..x-1]

factorize' :: Integer -> Integer -> [Integer]
factorize' 1 _ = []
factorize' x factor
    | x `mod` factor /= 0   = factorize' x $ head . dropWhile (<=factor) $ primes
    | otherwise = factor : factorize' (x `div` factor) factor

factorize :: Integer -> [Integer]
factorize x = factorize' x 2

-- combine two lists of prime factors. If a prime is in both lists, keep the
-- one to the higher power
combineFactors :: [Integer] -> [Integer] -> [Integer]
combineFactors xs [] = xs
combineFactors xs ys@(y:_)
  | (length . filter (==y) $ xs) > (length . filter (==y) $ ys) = combineFactors (xs ++ filter (/=y) ys) (filter (/=y) ys)
  | otherwise                                                   = combineFactors (filter (/=y) xs ++ filter (==y) ys) (filter (/=y) ys)

smallestMultiple :: Integer
smallestMultiple = foldr (*) 1 (foldr combineFactors [] $ map factorize [1..20])
