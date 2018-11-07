mdcE :: Integer -> Integer -> (Integer, Integer)
mdcE 0 b = (1, 0)
mdcE a b =
    let(t, s) = mdcE (b `mod` a) a
    in (s, t - (b `div` a) * s)
