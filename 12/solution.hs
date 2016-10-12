import Data.List

triangle :: Int -> Int
triangle n = n * ( n + 1) `div` 2

factors' :: Int -> Int -> Int-> Int
factors' n x c
  | n < x * x       = c
  | n == x          = 1 + c
  | n `mod` x == 0  = factors' n (x+1) (c + 2)
  | otherwise       = factors' n (x + 1) c

factors :: Int -> Int
factors n = factors' n 1 0

hundredFactors :: Int
hundredFactors = head $ dropWhile(\x -> factors x < 500) (map triangle [1..])

main :: IO ()
main = putStrLn $ show hundredFactors
