module Geometry

tupleList :: Int -> [(Int, Int)]
tupleList n = [(x,x) | x <- [1..n]]

thetas :: [(Int, Int)] -> [(Double, Double)]
thetas l = [(2 * pi / fromIntegral x, 2 * pi / fromIntegral y) | (x,y) <- l]

pxy :: Double -> [(Double, Double)] -> [(Double, Double)]
pxy r l = [(r * (cos x), r * (sin y)) | (x, y) <- l]

adjCenter ::(Double, Double) ->  [(Double, Double)] -> [(Double, Double)]
adjCenter c l = [(fst c + x, snd c + y) | (x, y) <- l]

getNPointsOnCircle r n c = adjCenter c (pxy r (thetas(tupleList(n))))
