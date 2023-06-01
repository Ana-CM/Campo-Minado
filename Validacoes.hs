{-
    Ana Carolina Mendes Lino - 201865212AC
    Paula Rinco Rodrigues Pereira - 201865559C
-}

module Validacoes where

-- Importações
import Utilitarios
import Text.Read (readMaybe)
import Data.Char (ord)
import Tabuleiro

-- Método responsável por validar inteiro, caso não seja um inteiro, retorna 26
validarInteiroEntrada :: String -> Int
validarInteiroEntrada entrada =
  case readMaybe entrada of
    Just numero -> numero
    Nothing -> 26

-- Método responsável por validar o tamanho do tabuleiro
validarTamanhoTabuleiro :: Int -> Int
validarTamanhoTabuleiro tam
  | tam < 2 = 26
  | tam > 26 = 26
  | otherwise = tam

-- Método responsável por validar a quantidade de bombas
validarQuantidadeBombas :: Int -> Int -> Int
validarQuantidadeBombas bombas tam
  | bombas < 1 = 1
  | bombas > div tam 2 = div tam 2
  | otherwise = bombas

-- Metodo responsavel por validar se caracter é uma letra
validarLetra :: Char -> Bool
validarLetra letra = ord letra >= ord 'a' && ord letra <= ord 'z'

-- Método para validar se uma string é um inteiro
validarInteiro :: String -> Bool
validarInteiro entrada =
  case (readMaybe entrada :: Maybe Int) of
    Just numero -> True
    Nothing -> False

-- Método responsável por validar a jogada escolhida pelo usuário
validarJogada :: String -> Int -> IO (String, Int, Int)
validarJogada jogada tamanho = do
  let tamanhoString = length jogada

  if tamanhoString >= 2 && tamanhoString <= 4
    then do
      let (primeiro, fim) = separarString jogada
      case primeiro of
        '-' -> do
          let (linha, coluna) = separarString fim
          if validarLetra linha && validarInteiro coluna
            then do
              let colunaInt = converterLetraParaNumero linha
              let linhaInt = converterStringParaInt coluna
              if linhaInt <= tamanho && colunaInt <= tamanho
                then return ("desmarcar", linhaInt, colunaInt)
              else return ("invalido", 0, 0)
          else return ("invalido", 0, 0)
        '+' -> do
          let (linha, coluna) = separarString fim
          if validarLetra linha && validarInteiro coluna
            then do
              let colunaInt = converterLetraParaNumero linha
              let linhaInt = converterStringParaInt coluna
              if linhaInt <= tamanho && colunaInt <= tamanho
                then return ("marcar", linhaInt, colunaInt)
              else return ("invalido", 0, 0)
          else return ("invalido", 0, 0)
        _ -> do
          if validarLetra primeiro && validarInteiro fim
            then do
              let colunaInt = converterLetraParaNumero primeiro
              let linhaInt = converterStringParaInt fim
              if linhaInt <= tamanho && colunaInt <= tamanho
                then return ("abrir", linhaInt, colunaInt)
              else return ("invalido", 0, 0)
          else return ("invalido", 0, 0)
      
  else return ("invalido", 0, 0)
