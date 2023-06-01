{-
    Ana Carolina Mendes Lino - 201865212AC
    Paula Rinco Rodrigues Pereira - 201865559C
-}

module Tabuleiro where

-- Importações
import System.Random
import Data.List
import Data.Char (chr, ord)

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

-- Método para encontrar uma posição no tabuleiro
encontraPosicao :: Int -> Int -> Tabuleiro -> Maybe Posicao
encontraPosicao linha' coluna' tabuleiro =
    find (\pos -> linha pos == linha' && coluna pos == coluna') tabuleiro

-- Método para retornar se a posição está aberta 
estaAberta :: Int -> Int -> Tabuleiro -> Bool
estaAberta linha coluna tabuleiro =
    let posicao = encontraPosicao linha coluna tabuleiro
    in case posicao of
        Nothing -> False
        Just pos -> aberta pos

-- Método para retornar se a posição está marcada como bomba
estaMarcada :: Int -> Int -> Tabuleiro -> Bool
estaMarcada linha coluna tabuleiro =
    let posicao = encontraPosicao linha coluna tabuleiro
    in case posicao of
        Nothing -> False
        Just pos -> marcada pos

-- Método para retornar se a posição é uma bomba
ehBomba :: Int -> Int -> Tabuleiro -> Bool
ehBomba linha coluna tabuleiro =
    let posicao = encontraPosicao linha coluna tabuleiro
    in case posicao of
        Nothing -> False
        Just pos -> bomba pos

-- Método que verifica se o total máximo de marcações foi atingido
totalMarcacoesAtingido :: Tabuleiro -> Int -> Bool
totalMarcacoesAtingido tabuleiro tamanho = length (filter marcada tabuleiro) >= (tamanho * tamanho) `div` 2

-- Método que abre uma posição no tabuleiro
abrirPosicaoTabuleiro :: Int -> Int -> Tabuleiro -> Tabuleiro
abrirPosicaoTabuleiro linha coluna tabuleiro =
    let posicao = encontraPosicao linha coluna tabuleiro
    in case posicao of
        Nothing -> tabuleiro
        Just pos ->
            let indice = elemIndex pos tabuleiro
            in case indice of
                Nothing -> tabuleiro
                Just idx ->
                    let (antes, depois) = splitAt idx tabuleiro
                        novasPosicoes = antes ++ (abrirPosicaoPosicao pos : tail depois)
                    in novasPosicoes

-- Método que abre uma posição
abrirPosicaoPosicao :: Posicao -> Posicao
abrirPosicaoPosicao posicao = posicao { aberta = True }

-- Método que marca bomba em tabuleiro
marcarBombaTabuleiro :: Int -> Int -> Tabuleiro -> Tabuleiro
marcarBombaTabuleiro linha coluna tabuleiro = 
    let posicao = encontraPosicao linha coluna tabuleiro
    in case posicao of
        Nothing -> tabuleiro
        Just pos ->
            let indice = elemIndex pos tabuleiro
            in case indice of
                Nothing -> tabuleiro
                Just idx ->
                    let (antes, depois) = splitAt idx tabuleiro
                        novasPosicoes = antes ++ (marcarBombaPosicao pos : tail depois)
                    in novasPosicoes

-- Método que marca uma posição como bomba
marcarBombaPosicao :: Posicao -> Posicao
marcarBombaPosicao posicao = posicao { marcada = True }

-- Método que desmarca bomba no tabuleiro
desmarcarBombaTabuleiro :: Int -> Int -> Tabuleiro -> Tabuleiro
desmarcarBombaTabuleiro linha coluna tabuleiro = 
    let posicao = encontraPosicao linha coluna tabuleiro
    in case posicao of
        Nothing -> tabuleiro
        Just pos ->
            let indice = elemIndex pos tabuleiro
            in case indice of
                Nothing -> tabuleiro
                Just idx ->
                    let (antes, depois) = splitAt idx tabuleiro
                        novasPosicoes = antes ++ (desmarcarBombaPosicao pos : tail depois)
                    in novasPosicoes

-- Método que desmarca uma posição como bomba
desmarcarBombaPosicao :: Posicao -> Posicao
desmarcarBombaPosicao posicao = posicao { marcada = False }

-- Método revelar tabuleiro
revelarTabuleiro :: Tabuleiro -> Int -> IO ()
revelarTabuleiro tabuleiro tamanho = do
    putStrLn ""
    putStrLn ("     " ++ concat (map (\x -> return (chr (x + ord 'A' - 1)) ++ " | ") [1..tamanho]))

    let imprimirValor posicao
            | bomba posicao = "B"
            | otherwise = show (valor posicao) 
    
    putStrLn (concat (map (\x -> if (coluna x) == 0 then "\n" ++ show ((linha x) + 1) ++ "    " ++ imprimirValor x ++ " | " else imprimirValor x ++ " | ") tabuleiro))

    -- Descomente caso deseja imprimir as posições exibindo a linha e coluna de cada posição
   {- putStrLn (concat (map (\x -> if (coluna x) == 0 then "\n" ++ show (linha x) ++ "    " ++ imprimirValor x ++ " (" ++ show(linha x) ++ "," ++ show(coluna x) ++ ") | " else imprimirValor x ++ " (" ++ show(linha x) ++ "," ++ show(coluna x) ++ ") | ") tabuleiro)) -}


-- Método para imprimir o tabuleiro para o usuário
imprimirTabuleiro :: Tabuleiro -> Int -> IO ()
imprimirTabuleiro tabuleiro tamanho = do
    putStrLn ""
    putStrLn ("     " ++ concat (map (\x -> return (chr (x + ord 'A' - 1)) ++ " | ") [1..tamanho]))

    let imprimirValor posicao
            | aberta posicao = show (valor posicao)
            | marcada posicao = "B"
            | otherwise = "*"

    putStrLn (concat (map (\x -> if (coluna x) == 0 then "\n" ++ show ((linha x) + 1) ++ "    " ++ imprimirValor x ++ " | " else imprimirValor x ++ " | ") tabuleiro))
  -- Descomente caso deseja imprimir as posições exibindo a linha e coluna de cada posição
  --  putStrLn (concat (map (\x -> if (coluna x) == 0 then "\n" ++ show ((linha x) + 1) ++ "    " ++ imprimirValor x ++ " (" ++ show(linha x) ++ "," ++ show(coluna x) ++ ") | " else imprimirValor x ++ " (" ++ show(linha x) ++ "," ++ show(coluna x) ++ ") | ") tabuleiro))
