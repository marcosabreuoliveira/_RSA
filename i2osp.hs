i2osp :: Integer -> [Intege]
i2osp x
    | x <= 0 = []
    | otherwise = x `mod` (toInteger 256) : i2osp (x `div` (toInteger 256))