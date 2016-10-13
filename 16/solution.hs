import Data.Char ( digitToInt )
digits :: Integer -> Int
digits n = foldl (+) 0 result
    where result = map (\x-> digitToInt x) $ show n

main :: IO ()
main = putStrLn $ show $ digits (2^1000)
