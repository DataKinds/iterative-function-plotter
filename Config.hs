module Config where
    import Data.Complex
    amountX :: Integer
    amountX = 10
    maxX :: Double
    maxX = 4
    minX :: Double
    minX = -4
    amountY :: Integer
    amountY = 10
    maxY :: Double
    maxY = 4
    minY :: Double
    minY = -4

    imageHeight :: Int
    imageHeight = 400
    imageWidth :: Int
    imageWidth = 400
    steps :: Int
    steps = 5

    znSucc :: Complex Double -> Complex Double -> Complex Double
    --znSucc c zn = (zn ** 2) + c
    --znSucc c zn = zn + (0.1 :+ 0.1)
    --znSucc c zn = zn ** 1.1
    znSucc c zn = zn ** 0.5
    buildZnSeries :: Int -> Complex Double -> [Complex Double]
    buildZnSeries n c = take n $ iterate (znSucc c) c
