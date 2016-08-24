digits :: [Integer]
digits = [999,998..100]

isPalindrome :: Integer -> Bool
isPalindrome x = string == reverse string
    where string = show x

largestPalindrome :: Integer
largestPalindrome = maximum . filter (isPalindrome) $ concatMap (\x -> concatMap(\y -> [x*y]) [x,x-1..1]) digits
