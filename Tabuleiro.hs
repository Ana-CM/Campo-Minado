{-
    Ana Carolina Mendes Lino - 201865212AC
    Paula Rinco Rodrigues Pereira - 201865559C
-}

module Tabuleiro where

-- Importações
import System.Random
import Data.List

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

-- Método responsável por gerar o tabuleiro
gerarTabuleiro :: Int -> Int -> IO Tabuleiro
gerarTabuleiro tamanho qtdBombas = do
    let posicoes = gerarPosicoes tamanho
    posicoesComBombas <- adicionarBombasAleatorias qtdBombas posicoes
    return posicoesComBombas

-- Método responsável por gerar as posições do tabuleiro
gerarPosicoes :: Int -> [Posicao]
gerarPosicoes tamanho =
    [Posicao x y False False False 0 | x <- [0..tamanho-1], y <- [0..tamanho-1]]

-- Método responsável por adicionar as bombas de forma aleatória
adicionarBombasAleatorias :: Int -> [Posicao] -> IO [Posicao]
adicionarBombasAleatorias 0 posicoes = return posicoes
adicionarBombasAleatorias qtdBombas posicoes = do
    maybePosicao <- sortearPosicao posicoes
    case maybePosicao of
        Nothing -> adicionarBombasAleatorias qtdBombas posicoes
        Just posicao -> do
            let novasPosicoes = (adicionarBomba posicao) : delete posicao posicoes
            adicionarBombasAleatorias (qtdBombas - 1) novasPosicoes

-- Método responsável por sortear uma posição
sortearPosicao :: [Posicao] -> IO (Maybe Posicao)
sortearPosicao posicoes = do
    indice <- randomRIO (0, length posicoes - 1)
    let posicao = posicoes !! indice
    if bomba posicao
        then return Nothing
        else return (Just posicao)

-- Método responsável por adicionar uma bomba em uma posição
adicionarBomba :: Posicao -> Posicao
adicionarBomba posicao = posicao { bomba = True }

-- Método responsável por revelar o tabuleiro
-- Método revelar tabuleiro (Por hora só mostra o valor de cada posição)
revelarTabuleiro :: Tabuleiro -> Int -> IO ()
revelarTabuleiro tabuleiro tamanho = do
    putStrLn ""
    putStrLn ("     " ++ concat (map (\x -> show x ++ " ") [0..tamanho-1]))
    putStrLn (concat (map (\x -> if (coluna x) == 0 then "\n" ++ show (linha x) ++ "    " ++ show (valor x) ++ " " else show (valor x) ++ " ") tabuleiro))
