## Goblin

### Resumo:

A linguagem Goblin possui como domínio a área de Business, com foco para o desenvolvimento de aplicações precisas e confiáveis, permitindo gerar relatórios a partir dos dados armazenados.

### Arquitetura:

Utiliza-se o Parsec e o Alex através da linguagem **Haskel** para definir os termos mais primitivos da linguagem e geral o executável.


### Execução:

É necessário instalar previamente a ferramenta `cabal` (podende estar presente na ferramenta `ghci`) e usá-lo para instalar o Alex e o Parsec.

Para instalar o `GHCup` (que instala `GHC`, `caba-install`, etc) no Linux, rode o seguinte comando:

```
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

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
ghc goblin_exec.hs lexer.hs primitive_tokens.hs -o parser
```

Finalmente, para o parser ler um arquivo de exemplo:

```
./parser program-readable.pe
```


### Adicionando "hightlight" sintático da linguagem ao Sublime
Para adicionar o _sintax highlight_ do Goblin no _Sublime_ basta colocar o arquivo `goblin.sublime-syntax` dentro da seguinte pasta no Linux:
```
home/<user_name>/.config/sublime-text-3/Packages/User/
```
