sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0,1)
sum'n'count x = (sum, count) where
  (sum, count) = digits (abs x) 0 0
  digits 0 s c = (s, c)
  digits n s c = digits (div n 10) (s + mod n 10 ) (c + 1)
