import Data.List    ( sortBy )
collatz :: Int -> [Int]
collatz n
    | n == 1    = [1]
    | n `mod` 2 == 0    = n : collatz (n `div` 2)
    | otherwise         = n : collatz ((3 * n) + 1)

sortZip :: (Int, Int) -> (Int, Int) -> Ordering
sortZip (size1, _) (size2, _) = compare size2 size1

main :: IO ()
main = putStrLn $ show $ head $ sortBy sortZip (zip (map (length . collatz) [1..1000000])[1..1000000])
