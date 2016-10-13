factorial :: Integer -> Integer
factorial n = foldl (*) 1 [1..n]

latticePath :: Integer -> Integer
latticePath r = (factorial (r * 2)) `div` ((factorial r) * (factorial r))

main :: IO ()
main = putStrLn $ show $ latticePath 20
