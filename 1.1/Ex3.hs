amount :: Double -> Int -> Double -> Double
amount start years interest | years == 0 = start
                            | otherwise = amount (start*(1+interest)) (years-1) interest
