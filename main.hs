module Main where

import Encrypt

main :: IO()
main = do
         -- readFile :: FilePath -> IO String
         msgm <- readFile "file.txt"

         primos <- rndPrimes 512
         let p = fst primos
         let q = snd primos
         let n = p * q
         e <- getPublicExponent p q
         let encoded = os2ip msgm
         let msgm_cript = encrypt encoded e n
         d <- getPrivateExponent e p q

         --escreve nos arquivos
         writeFile "chave_privada_d.txt" (show d)
         writeFile "chave_privada_n.txt" (show n)
         writeFile "txt_cript.txt" (show msgm_cript)

         --recupera dos arquivos
         txt_cript <- readFile "txt_cript.txt"
         chave_privada_d <- readFile "chave_privada_d.txt"
         chave_privada_n <- readFile "chave_privada_n.txt"

         let msgm_descript = i2osp(decrypt (read txt_cript :: Integer) (read chave_privada_d :: Integer) (read chave_privada_n :: Integer))
         writeFile "txt_descript.txt" (show msgm_descript)
