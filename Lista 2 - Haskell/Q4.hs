isEqTriangle :: [(Double, Double)] -> Bool
isEqTriangle [] = True
isEqTriangle [(x, y), (x1, y1), (x2, y2)] = (distance (x, y) (x1, y1) + 0.000000000000000000000001) >= distance (x, y) (x2, y2) && (distance (x, y) (x2, y2) - 0.000000000000000000001) <= distance (x1, y1) (x2, y2)
isEqTriangle points = False

distance :: (Double, Double) -> (Double, Double) -> Double
distance (x , y) (x1, y1) = sqrt (sqr(x1 - x) + sqr(y1 - y))

sqr :: Double -> Double
sqr x = x * x


