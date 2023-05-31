{-
    Ana Carolina Mendes Lino - 201865212AC
    Paula Rinco Rodrigues Pereira - 201865559C
-}

module Jogadas where

-- Importações
import Text.Read (readMaybe)
import Tabuleiro
import Validacoes
import Utilitarios

-- Método responsável por perguntar ao usuário a jogada desejada
perguntaJogada :: Int -> Tabuleiro -> IO ()
perguntaJogada tamanho tabuleiro = do
    putStrLn ""
    putStrLn "Insira a jogada desejada:"

    jogada <- getLine

    (validado, linha, coluna) <- validarJogada jogada tamanho

    let indiceLinha = linha - 1
    let indiceColuna = coluna - 1

    case validado of
        "abrir" -> do
            putStrLn ("Abrir " ++ jogada)
        "marcar" -> do
            putStrLn ("Marcar " ++ jogada)
        "desmarcar" -> do
            putStrLn ("Desmarcar " ++ jogada)
        _ -> do
            perguntaJogada tamanho tabuleiro

    imprimirTabuleiro tabuleiro tamanho

    putStrLn("verificar se o jogo acabou")
            
    perguntaJogada tamanho tabuleiro
