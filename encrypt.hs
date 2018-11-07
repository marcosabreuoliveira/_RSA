import Data.Bits
import System.Random
import Control.Monad.Fix
import Math.NumberTheory.Primes.Testing.Probabilistic
{-Retorna um número primo de nbits-}
rndPrime :: Int -> IO Integer
rndPrime bits =
  fix $ \again -> do
    x <- fmap (.|. 1) $ randomRIO (2^(bits - 1), 2^bits - 1)
    if isPrime x then return x else again

{-Retorna os primos (p,q) de tamanho nbits-}
rndPrimes :: Int -> IO (Integer, Integer)
rndPrimes bits = do
  p <- rndPrime bits
  fix $ \again -> do
    q <- rndPrime bits
    if p /= q then return (p, q) else again

{-Retorna o numero de co-primos -}
totiente :: Integer -> Integer -> Integer
totiente p q = (p-1) * (q - 1)

{-MDC com tratamento para entradas negativas-}
fullMod :: Integer -> Integer -> Integer
fullMod a b
    | a >= 0 = a `mod` b
    | otherwise = fullMod (a + b) b

{-Algoritmo de Euclides Estendido, usado para descobrir d-}
mdcE :: Integer -> Integer -> (Integer, Integer)
mdcE 0 b = (0, 1)
mdcE a b =
    let(s, t) = mdcE (b `mod` a) a
    in (t - (b `div` a) * s, s)

{-Retorna o 'd' da chave privada-}
getPrivateExponent :: Integer -> Integer -> Integer -> IO Integer
getPrivateExponent e p q =
        return (fullMod (fst (mdcE e (totiente p q)))
          (totiente p q))

{-Retorna o expoente publico 'e' da chave publica-}
getPublicExponent :: Integer -> Integer -> IO Integer
getPublicExponent p q =
    do
        e <- randomRIO(2, totiente p q - 1)
        if gcd e (totiente p q) == 1
            then return e
            else getPublicExponent p q

{-Modular Exponentiation: melhora o desempenho da operacao c^m mod n-}
modExp :: Integer -> Integer -> Integer -> Integer
modExp b 0 m = 1
modExp b e m = t * modExp ((b * b) `mod` m) (shiftR e 1) m `mod` m
  		   where t = if testBit e 0 then b `mod` m else 1

int2ascii :: [Int] -> [Char]
int2ascii n = [toEnum x :: Char | x <- n]

{-Transforma um inteiro em uma lista de caracteres-}
i2osp :: Integer -> [Integer]
i2osp x
  | x <= 0 = []
  | otherwise = x `mod` (toInteger 256) : i2osp (x `div` (toInteger 256))

{-Transforma uma lista de caracteres em um inteiro-}
os2ip teste = sum [(fromEnum (fst x)) * 256 ^ (snd x) | x <- (zip teste [0..(length teste -1)])]

encrypt :: Integer -> Integer -> Integer -> Integer
encrypt m e n = modExp m e n

decrypt :: Integer -> Integer -> Integer -> Integer
decrypt c d n = modExp c d n
