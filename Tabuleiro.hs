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
    posicoesComValores <- adicionarValores (tamanho * tamanho) posicoesComBombas
    return posicoesComValores 

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
            let indice = elemIndex posicao posicoes
            case indice of
                Nothing -> adicionarBombasAleatorias qtdBombas posicoes
                Just idx -> do
                    let (antes, depois) = splitAt idx posicoes
                        novasPosicoes = antes ++ (adicionarBomba posicao : tail depois)
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

-- Método responsável por adicionar os valores nas posições
adicionarValores :: Int -> [Posicao] -> IO [Posicao]
adicionarValores 0 posicoes = return posicoes
adicionarValores qtd posicoes = do
    let posicao = posicoes !! (qtd - 1)
    if bomba posicao
        then adicionarValores (qtd - 1) posicoes
        else do
            let indice = elemIndex posicao posicoes
            case indice of
                Nothing -> adicionarValores (qtd - 1) posicoes
                Just idx -> do
                    let (antes, depois) = splitAt idx posicoes
                        novasPosicoes = antes ++ (adicionarValor posicao posicoes : tail depois)
                    adicionarValores (qtd - 1) novasPosicoes

-- Método responsável por adicionar quantas bombas tem ao redor de uma posição
adicionarValor :: Posicao -> [Posicao] -> Posicao
adicionarValor posicao posicoes = 
    let posicoesAoRedor = posicoesAdjacentes posicao posicoes
        qtdBombas = length (filter bomba posicoesAoRedor)
    in posicao { valor = qtdBombas }

-- Método responsável por retornar as posições adjacentes a uma posição
posicoesAdjacentes :: Posicao -> [Posicao] -> [Posicao]
posicoesAdjacentes posicao posicoes = filter (\adj -> ehAdjacente posicao adj) posicoes
  where
    ehAdjacente p1 p2 =
        let dl = abs (linha p1 - linha p2)
            dc = abs (coluna p1 - coluna p2)
        in (dl == 1 && dc == 0) || (dl == 0 && dc == 1)
    
-- Método revelar tabuleiro
revelarTabuleiro :: Tabuleiro -> Int -> IO ()
revelarTabuleiro tabuleiro tamanho = do
    putStrLn ""
    putStrLn "Tabuleiro revelado:"
    putStrLn ("     " ++ concat (map (\x -> show x ++ " | ") [0..tamanho-1]))

    let imprimirValor posicao
            | bomba posicao = "B"
            | otherwise = show (valor posicao) 
    
    putStrLn (concat (map (\x -> if (coluna x) == 0 then "\n" ++ show (linha x) ++ "    " ++ imprimirValor x ++ " | " else imprimirValor x ++ " | ") tabuleiro))

    -- Descomente caso deseja imprimir as posições exibindo a linha e coluna de cada posição
   {- putStrLn (concat (map (\x -> if (coluna x) == 0 then "\n" ++ show (linha x) ++ "    " ++ imprimirValor x ++ " (" ++ show(linha x) ++ "," ++ show(coluna x) ++ ") | " else imprimirValor x ++ " (" ++ show(linha x) ++ "," ++ show(coluna x) ++ ") | ") tabuleiro)) -}


-- Método para imprimir o tabuleiro para o usuário
imprimirTabuleiro :: Tabuleiro -> Int -> IO ()
imprimirTabuleiro tabuleiro tamanho = do
    putStrLn ""
    putStrLn "Tabuleiro Parcial:"
    putStrLn ("     " ++ concat (map (\x -> show x ++ " | ") [0..tamanho-1]))

    let imprimirValor posicao
            | aberta posicao = show (valor posicao)
            | marcada posicao = "B"
            | otherwise = "*"

    putStrLn (concat (map (\x -> if (coluna x) == 0 then "\n" ++ show (linha x) ++ "    " ++ imprimirValor x ++ " | " else imprimirValor x ++ " | ") tabuleiro))

