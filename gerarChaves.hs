module Main where

import Encrypt
import System.IO

main :: IO ()
main = do
    --RSA
    primos <- rndPrimes 512
    let p = fst primos
    let q = snd primos
    let n = p * q
    e <- getPublicExponent p q
    d <- getPrivateExponent e p q

    --gera as chaves privadas
    writeFile "chave_privada_d.txt" (show d)
    writeFile "chave_privada_n.txt" (show n)

    --gera as chaves publicas
    writeFile "chave_publica_e.txt" (show e)
    writeFile "chave_publica_n" (show n)
