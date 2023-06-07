# Campo Minado

Essa é uma implementação do jogo do campo minado em haskell usando o GHCi como interpretador (versão 8.6.5).

## Como compilar

Para compilar o jogo, basta executar os seguintes comandos no terminal:

```bash
ghci main.hs
```

```bash
main
```
## Como jogar

O jogador deverá informar o tamanho do tabuleiro, sendo esse valor um número maior que 1 e menor ou igual a 26. \
\
Em seguida, o jogador deverá informar a quantidade de minas que deseja espalhar pelo tabuleiro, sendo esse valor um número maior que 0 e menor que a metade da quantidade de casas do tabuleiro. \
\
Se em um dos dois casos acima, o jogador informar um valor inválido, o jogo irá escolher o valor extremo, valido, mais próximo do valor informado. Por exemplo, se o jogador informar o valor 50 para o tamanho do tabuleiro, o jogo irá considerar o valor 26.\
\
Após isso, o jogador deverá informar o tipo de jogada e a posição da jogada. 
\
Os tipos de jogada são: \
\
1 - Abrir uma casa: o jogador deverá usar uma letra para representar a coluna e um número para representar a linha. Por exemplo, se o jogador quiser abrir a casa da primeira linha e da segunda coluna, ele deverá informar a letra 'b' e o número 1 (b1). \
2 - Marcar uma casa: o jogador deverá usar o operador '+' seguido de uma letra para representar a coluna e um número para representar a linha. Por exemplo, se o jogador quiser marcar a casa segunda linha e da terceira coluna, ele deverá informar o operador '+' seguido da letra 'c' e do número 2 (+c2). \
3 - Desmarcar uma casa: o jogador deverá usar o operador '-' seguido de uma letra para representar a coluna e um número para representar a linha. Por exemplo, se o jogador quiser desmarcar a casa da terceira linha e da quarta coluna, ele deverá informar o operador '-' seguido da letra 'd' e do número 3 (-d3). 

Se em algum momento o jogador informar uma jogada inválida, o jogo irá pedir para que o jogador informe uma jogada válida. 

## Terminando o jogo

O jogo termina caso o jogador abra uma casa que tenha uma mina (perdendo o jogo) ou caso o jogador abra todas as casas que não tenham minas e marque todas as casas que tenham minas (vencendo o jogo). 
