sumSquareDifference :: Integer -> Integer
sumSquareDifference x   = squareSum - sumSquare
    where sumSquare     = foldr (\x -> (x^2 +)) 0 [1..x]
          squareSum     = sum [1..x] ^ 2

sumSquareDifference' :: Integer -> Integer
sumSquareDifference' x   = squareSum - sumSquare
    where sumSquare = x * (x + 1) * (2 * x + 1) `div` 6
          squareSum = (x * (x + 1) `div` 2) ^ 2
          
