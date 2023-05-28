{-
    Ana Carolina Mendes Lino - 201865212AC
    Paula Rinco Rodrigues Pereira - 201865559C
-}

module Main where

-- Importações
import Validacoes
import Tabuleiro


-- Método responsável por iniciar o jogo "CAMPO MINADO"
main :: IO ()
main = do
    putStrLn "Insira o tamanho do tabuleiro:"
    tam <- getLine
    let tamanho = validarTamanhoTabuleiro (validarInteiro tam)

    putStrLn "Insira a quantidade de bombas tabuleiro:"
    bombas <- getLine
    let qtdBombas = validarQuantidadeBombas (validarInteiro bombas) (tamanho*tamanho)

    tabuleiro <- gerarTabuleiro tamanho qtdBombas

    imprimirTabuleiro tabuleiro tamanho
    revelarTabuleiro tabuleiro tamanho

    putStrLn ""
    putStrLn ("Tamanho do tabuleiro: " ++ show tamanho)
    putStrLn ("Quantidade de bombas: " ++ show qtdBombas)

    