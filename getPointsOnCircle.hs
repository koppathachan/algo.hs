nPtsOnCircle :: Double -> Int -> (Double, Double) -> [(Double, Double)] 
nPtsOnCircle r n c = 
    [(fst c + (r * (cos (fromIntegral x * (pi / 2)))), snd c + (r * (sin (fromIntegral x * (pi / 2))))) 
        | x <- [1..n]]
