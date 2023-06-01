{-
    Ana Carolina Mendes Lino - 201865212AC
    Paula Rinco Rodrigues Pereira - 201865559C
-}

module Main where

-- Importações
import Validacoes
import Tabuleiro
import Jogadas


-- Método responsável por iniciar o jogo "CAMPO MINADO"
main :: IO ()
main = do
    putStrLn "Insira o tamanho do tabuleiro:"
    tam <- getLine
    let tamanho = validarTamanhoTabuleiro (validarInteiroEntrada tam)

    putStrLn "Insira a quantidade de bombas tabuleiro:"
    bombas <- getLine
    let qtdBombas = validarQuantidadeBombas (validarInteiroEntrada bombas) (tamanho*tamanho)

    tabuleiro <- gerarTabuleiro tamanho qtdBombas
    
    revelarTabuleiro tabuleiro tamanho

    putStrLn ""
    putStrLn "Bem vindo ao Campo Minado!"

    imprimirTabuleiro tabuleiro tamanho
    perguntaJogada tamanho tabuleiro
    