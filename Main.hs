import Data.Complex
import Numeric
import Config

type Grid = [Complex Double]
type GridSeries = [[Complex Double]]

lerp :: Double -> Double -> Double -> Double
lerp low high t
    | 0.0 <= t && t <= 1.0 = low + t * (high - low)
    | t < 0.0 = low
    | t > 1.0 = high

redistribute :: Double -> Double -> Double -> Double -> Double -> Double
redistribute a0 a1 b0 b1 n = ((n - a0) / (a1 - a0)) * (b1 - b0) + b0

intervalX :: [Double]
intervalX = [lerp minX maxX ((fromIntegral x) / (fromIntegral amountX)) | x <- [0 .. amountX]]
intervalY :: [Double]
intervalY = [lerp minY maxY ((fromIntegral y) / (fromIntegral amountY)) | y <- [0 .. amountY]]
baseGrid :: Grid
baseGrid = [x :+ y | x <- intervalX, y <- intervalY]

znSeries :: GridSeries
znSeries = map (buildZnSeries steps) baseGrid

correctXCoord :: Double -> Double
correctXCoord x = redistribute minX maxX 0.0 (fromIntegral imageWidth) x
correctYCoord :: Double -> Double
correctYCoord y = redistribute minY maxY (fromIntegral imageHeight) 0.0 y
svgHeader :: String
svgHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n\
\<svg\n\
\   xmlns:dc=\"http://purl.org/dc/elements/1.1/\"\n\
\   xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\n\
\   xmlns:svg=\"http://www.w3.org/2000/svg\"\n\
\   xmlns=\"http://www.w3.org/2000/svg\"\n\
\   width=\"" ++ show imageHeight ++ "\"\n\
\   height=\"" ++ show imageWidth ++ "\"\n\
\   version=\"1.1\"\n\
\   id=\"svg8\">\n\
\<rect y=\"0\" x=\"0\" height=\"100%\" width=\"100%\" id=\"bg\" style=\"fill:#000000;fill-opacity:1\" />"
svgFooter :: String
svgFooter = "</svg>"
svgDot :: Double -> Double -> String
svgDot x y = "<circle r=\"3\" cx=\"" ++ show (correctXCoord x) ++ "\" cy=\"" ++ show (correctYCoord y) ++ "\" style=\"fill:#0000ff;fill-opacity:1;\" />"
svgLine :: Double -> Double -> Double -> Double -> String -> String
svgLine x0 y0 x1 y1 color = "<line x1=\"" ++ show (correctXCoord x0) ++ "\" y1=\"" ++ show (correctYCoord y0) ++ "\" x2=\"" ++ show (correctXCoord x1) ++ "\" y2=\"" ++ show (correctYCoord y1) ++ "\" style=\"stroke:#" ++ color ++ ";stroke-width:1px;stroke-opacity:1\"/>"

colorSeries :: [String]
colorSeries = map (hexConv) colorRange
    where
        colorRange = map (floor . (redistribute 1.0 (fromIntegral steps) 10.0 255.0) . fromIntegral) [1 .. steps]
        hexConv n = if n < 16 then "0" ++ showHex n "" else showHex n ""

data ColoredLine = ColoredLine {fstPoint  :: Complex Double,
                                sndPoint  :: Complex Double,
                                lineColor :: String}

overlappedZnSeries :: [ColoredLine]
overlappedZnSeries = concat $ map (\ps -> zipWith3 (ColoredLine) ps (tail ps) (reverse colorSeries)) znSeries
lineConnectingZnTuples :: ColoredLine -> String
lineConnectingZnTuples ps = svgLine (realPart $ fstPoint ps) (imagPart $ fstPoint ps) (realPart $ sndPoint ps) (imagPart $ sndPoint ps) (lineColor ps)
svgLines :: String
svgLines = unlines $ map (lineConnectingZnTuples) overlappedZnSeries
svgDots :: String
svgDots = unlines [svgDot (realPart p) (imagPart p) | ps <- znSeries, p <- ps]

svgBody :: String
--svgBody = unlines [svgDots, svgLines]
svgBody = svgLines
fullSvgImage :: String
fullSvgImage = unlines [ svgHeader,
                         svgBody,
                         svgFooter ]
main :: IO ()
main = putStrLn fullSvgImage
