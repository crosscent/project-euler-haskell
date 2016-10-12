pythagoreans :: [(Integer, Integer, Integer)]
pythagoreans = [ (x, y, floor. sqrt . fromIntegral $ (x^2 + y^2)) | x <- [1..1000], y <-[1..1000], (fromIntegral (x + y)) + (sqrt . fromIntegral $(x^2 + y^2)) == 1000]
