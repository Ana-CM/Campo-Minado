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
import Debug.Trace

-- Método responsável por perguntar ao usuário a jogada desejada
perguntaJogada :: Int -> Tabuleiro -> IO ()
perguntaJogada tamanho tabuleiro = do
    putStrLn ""
    putStrLn "Insira a jogada desejada:"

    jogada <- getLine

    (validado, linha, coluna) <- validarJogada jogada tamanho

    let indiceLinha = linha - 1
    let indiceColuna = coluna - 1

    putStrLn ("Linha: " ++ show indiceLinha)
    putStrLn ("Coluna: " ++ show indiceColuna)

    case validado of
        "abrir" -> do
            putStrLn ("Abrir " ++ jogada)
        "marcar" -> do
            let novoTabuleiro = marcarBomba indiceLinha indiceColuna tabuleiro tamanho
            putStrLn ("Marcar " ++ jogada)
            imprimirTabuleiro novoTabuleiro tamanho
            -- verificar se o jogo acabou
            perguntaJogada tamanho novoTabuleiro
        "desmarcar" -> do
            putStrLn ("Desmarcar " ++ jogada)
        _ -> do
            let novoTabuleiro = tabuleiro
            putStrLn ("Jogada inválida! " ++ jogada)
            imprimirTabuleiro novoTabuleiro tamanho
            perguntaJogada tamanho novoTabuleiro

-- Método responsável por marcar uma posição no tabuleiro como bomba
marcarBomba :: Int -> Int -> Tabuleiro -> Int-> Tabuleiro
marcarBomba linha coluna tabuleiro tamanho =
    if (not (estaAberta linha coluna tabuleiro)) && (not (estaMarcada linha coluna tabuleiro)) && (not (totalMarcacoesAtingido tabuleiro tamanho))
        then 
            marcarBombaTabuleiro linha coluna tabuleiro
    else
        tabuleiro