mdc :: Integer -> Integer -> Bool
mdc x y
    | gcd x y == 1 = True
    | otherwise = False