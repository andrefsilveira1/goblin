## Goblin

### Resumo:

A linguagem Goblin possui como domínio a área de Business, com foco para o desenvolvimento de aplicações precisas e confiáveis, permitindo gerar relatórios a partir dos dados armazenados.

### Arquitetura:

Utiliza-se o Parsec e o Alex através da linguagem **Haskel** para definir os termos mais primitivos da linguagem e geral o executável.


### Execução:

É necessário instalar previamente a ferramenta `cabal` (podende estar presente na ferramenta `ghci`) e usá-lo para instalar o Alex e o Parsec.

```
cabal install alex
cabal install parsec
```
Depois, para deixar a parte léxica disponível para o parsec, digite o comando abaixo:

```
alex lexer.x
```

Para compilar o parser:

```
ghc goblin_exec.hs lexer.hs -o parser
```

Finalmente, para o parser ler uma arquivo de exemplo:

```
./parser program.pe
```

Se quiser mudar o arquivo de exemplo, para `program-readable.pe`, por exemplo, é necessário alterar o parâmetro do último parser do arquvo `goblin_exec.hs` para `program-readable.pe`, recompilar o arquivo do parser e rodar o seguinte comando:

```
./parser program-readable.pe
```
