{-
    Ana Carolina Mendes Lino - 201865212AC
    Paula Rinco Rodrigues Pereira - 201865559C
-}

module Validacoes where

-- Importações
import Text.Read (readMaybe)

-- Método responsável por validar inteiro, caso não seja um inteiro, retorna 5
validarInteiro :: String -> Int
validarInteiro entrada =
  case readMaybe entrada of
    Just numero -> numero
    Nothing -> 5

-- Método responsável por validar a quantidade de bombas
validarQuantidadeBombas :: Int -> Int -> Int
validarQuantidadeBombas bombas tam
  | bombas < 1 = 1
  | bombas > div tam 2 = div tam 2
  | otherwise = bombas
