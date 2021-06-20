reducer x y 0 = x
reducer x y n | n > 0 = reducer y (x + y) (n - 1)
              | n < 0 = reducer (y - x) x (n + 1)

fibonacci :: Integer -> Integer
fibonacci = reducer 0 1
