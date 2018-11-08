module Main where

import Encrypt
import System.IO

main :: IO ()
main = do
    --carrega os arquivos
    --mansagem a ser criptografada
    arquivo_mensagem <- readFile "mensagem.txt"
    --chaves publicas
    chave_publica_e <- readFile "chave_publica_e.txt"
    chave_publica_n <- readFile "chave_publica_n.txt"

    --criptografa ela
    let encoded = os2ip arquivo_mensagem
    let mensagem_criptografada = encrypt encoded (read chave_publica_e :: Integer) (read chave_publica_n :: Integer)

    --escreve no txt
    writeFile "txt_cript.txt" (show mensagem_criptografada)
