integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = sum 0 0 where
  step :: Double
  step = (b - a) / 1000
  x :: Integer -> Double
  x i = a + fromInteger i * step
  sum :: Integer -> Double -> Double
  sum 1000 acc = acc
  sum i acc = sum (i + 1) (acc + (f (x i) + f (x (i + 1))) * (x (i + 1) - x i) / 2.0)
