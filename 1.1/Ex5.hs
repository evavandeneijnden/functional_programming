extrX :: Double -> Double -> Double -> Double
extrX a b c = -b/2*a

extrY :: Double -> Double -> Double -> Double
extrY a b c = a*(extrX a b c)^2 + b*(extrX a b c) + c
