module Main where

import Encrypt
import System.IO

main :: IO ()
main = do
    -- carrega as chaves
    chave_privada_d <- readFile "chave_privada_d.txt"
    chave_privada_n <- readFile "chave_privada_n.txt"

    -- carrega o texto criptografado e pra onde ele vai, essa forma é diferente da de cima pq da pra usar o hPutStr
    -- ai o texto vem formatado certinho
    arquivo_entrada <- openFile "txt_cript.txt" ReadMode
    arquivo_saida <- openFile "txt_descript.txt" WriteMode
    texto_entrada <- hGetContents arquivo_entrada

    --descriptografa e armazena em msgm_descript
    let msgm_descript = i2osp(decrypt (read texto_entrada :: Integer) (read chave_privada_d :: Integer) (read chave_privada_n :: Integer))

    --escreve o texto descriptografado no txt
    hPutStr arquivo_saida (msgm_descript)
    hClose arquivo_entrada
    hClose arquivo_saida
