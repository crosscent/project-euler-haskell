toDigits :: Integer -> [Integer]
toDigits x
    | x < 9    = [x]
    | otherwise = (toDigits (x `div` 10)) ++ [x `mod` 10]

largestProduct' :: [Integer] -> [Integer]
largestProduct' xs
    | length xs < 13    = []
    | otherwise         = (product . take 13 $ xs) : largestProduct' (tail xs)

largestProduct :: Integer -> Integer
largestProduct x = maximum $ largestProduct' $ toDigits x
