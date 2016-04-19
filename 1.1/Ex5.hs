extrX :: Double -> Double -> Double
extrX a b = -b/2*a

extrY :: Double -> Double -> Double -> Double
extrY a b c = a*(extrX a b)^2 + b*(extrX a b) + c
