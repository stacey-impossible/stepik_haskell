seqA :: Integer -> Integer
seqA n
  | n == 0 = 1
  | n == 1 = 2
  | n == 2 = 3
  | n > 2 = let
    reducer x y z 0 = x
    reducer x y z n = reducer y z (z + y - 2 * x) (n - 1)
  in reducer 1 2 3 n
  | otherwise = error "n must be non-negative"
