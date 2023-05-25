Método para perguntar o tamanho do tabuleiro

tamanho_tabuleiro = do
    putStrLn "Insira o tamanho do tabuleiro:"
    tamanho <- getLine 
    return tamanho


Método para perguntar quantidade de bombas

qtd_bombas = do
    putStrLn "Insira a quantidade de bombas:"
    bombas <- getLine
    if bombas > n
        then return n
    if bombas < 1
        then return 1

    return bombas



Método que pergunta qual o tipo de jogada

tipo_jogada = do
    putStrLn "Escolha a jogada:"
    putStrLn "Insira x para abrir uma posição:"
    putStrLn "Insira y para marcar uma posição como bomba:"
    putStrLn "Insira z para desmarcar uma posição marcada como bomba:"
    jogada <- getLine

    if jogada == x
        then return x
    if jogada == y
        then return y
    if jogada == z
        then return z

    imprime o tabuleiro
    retorna a função recursivamente? não sei como fazer