import Data.Char
i2osp :: Integer -> [Char]
i2osp x
    | x <= 0 = []
    | otherwise = chr(fromInteger  (x `mod` (toInteger 256))) : i2osp (x `div` (toInteger 256))
