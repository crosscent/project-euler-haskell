isPrime :: Integer -> Bool
isPrime x = length [ y | y <- factors x , x `mod` y == 0] == 0

factors :: Integer -> [Integer]
factors x = trials ++ map (x `div`) (reverse trials)
    where trials = [ y | y <- [2..floor . sqrt . fromIntegral $ x], x `mod` y == 0]

largestPrimeFactor :: Integer -> Integer
largestPrimeFactor x = last [y | y <- factors x, isPrime y]
