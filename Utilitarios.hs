{-
    Ana Carolina Mendes Lino - 201865212AC
    Paula Rinco Rodrigues Pereira - 201865559C
-}

module Utilitarios where

-- Importações
import Data.Char (ord)

--Método para converter uma letra para seu número correspondente no alfabeto
converterLetraParaNumero :: Char -> Int
converterLetraParaNumero letra = ord letra - ord 'a' + 1

--Método para converter uma string para um inteiro
converterStringParaInt :: String -> Int
converterStringParaInt string = read string :: Int

--Método para pegar o primeiro caracter de uma string
separarString :: String -> (Char, String)
separarString string = (head string, tail string)