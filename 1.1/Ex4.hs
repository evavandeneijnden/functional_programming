discr :: Double -> Double -> Double -> Double
discr a b c = b*b - 4*a*c

root1 :: Double -> Double -> Double -> Double
root1 a b c | discr a b c < 0 = error "negative discriminant"
            | discr a b c == 0 = -b / 2*a
            | discr a b c > 0 = (-b - sqrt(discr a b c))/2*a
root2 :: Double -> Double -> Double -> Double
root2 a b c | discr a b c < 0 = error "negative discriminant"
            | discr a b c == 0 = error "no second root with zero-discriminant"
            | discr a b c > 0 = (-b + sqrt(discr a b c))/2*a
