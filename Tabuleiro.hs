{-
    Ana Carolina Mendes Lino - 201865212AC
    Paula Rinco Rodrigues Pereira - 201865559C
-}

module Tabuleiro where

-- Importações
import System.Random
import Data.List
import Data.Maybe


-- Tipo de dado que representa uma posição do tabuleiro
data Posicao = Posicao {
    linha :: Int, 
    coluna :: Int, 
    bomba :: Bool, 
    aberta :: Bool, 
    marcada :: Bool, 
    valor :: Int
} deriving (Show, Eq)

-- Tipo de dado que representa o tabuleiro
type Tabuleiro = [Posicao]

-- Método responsável por gerar um tabuleiro
gerarTabuleiro :: Int -> Int -> Tabuleiro
gerarTabuleiro tamanho qtdBombas = 
    let
        posicoes = gerarPosicoes tamanho
    in 
        posicoes

-- Método responsável por gerar as posições do tabuleiro
gerarPosicoes :: Int -> [Posicao]
gerarPosicoes tamanho = 
    [Posicao x y False False False 0 | x <- [0..tamanho-1], y <- [0..tamanho-1]]


-- Método revelar tabuleiro (Por hora só mostra o valor de cada posição)
revelarTabuleiro :: Tabuleiro -> Int -> IO ()
revelarTabuleiro tabuleiro tamanho = do
    putStrLn ""
    putStrLn ("     " ++ concat (map (\x -> show x ++ " ") [0..tamanho-1]))
    putStrLn (concat (map (\x -> if (coluna x) == 0 then "\n" ++ show (linha x) ++ "    " ++ show (valor x) ++ " " else show (valor x) ++ " ") tabuleiro))

    

