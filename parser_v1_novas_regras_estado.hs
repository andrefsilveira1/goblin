module Main (main) where

import Lexer
import Text.Parsec

-- parsers para os tokens

programToken = tokenPrim show update_pos get_token where
  get_token Program = Just Program
  get_token _       = Nothing

idToken = tokenPrim show update_pos get_token where
  get_token (Id x) = Just (Id x)
  get_token _      = Nothing

varToken = tokenPrim show update_pos get_token where
  get_token Var = Just Var
  get_token _   = Nothing  

beginToken = tokenPrim show update_pos get_token where
  get_token Begin = Just Begin
  get_token _     = Nothing

endToken = tokenPrim show update_pos get_token where
  get_token End = Just End
  get_token _   = Nothing

semiColonToken :: Parsec [Token] st Token
semiColonToken = tokenPrim show update_pos get_token where
  get_token SemiColon = Just SemiColon
  get_token _         = Nothing

colonToken = tokenPrim show update_pos get_token where
  get_token Colon = Just Colon
  get_token _     = Nothing

assignToken = tokenPrim show update_pos get_token where
  get_token Assign = Just Assign
  get_token _      = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token (Int x) = Just (Int x)
  get_token _       = Nothing

typeToken = tokenPrim show update_pos get_token where
  get_token (Type x) = Just (Type x)
  get_token _        = Nothing 

update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  

-- parsers para os não-terminais

program :: Parsec [Token] [(Token,Token)] [Token]
program = do
            a <- programToken 
            b <- idToken 
            c <- varToken
            d <- varDecl
            e <- beginToken 
            f <- stmts
            g <- endToken
            eof
            return (a:b:[c] ++ d++ [e] ++ f ++ [g])

varDecl :: Parsec [Token] [(Token,Token)] [Token]
varDecl = do
            a <- idToken
            b <- colonToken
            c <- typeToken
            updateState(symtable_insert (a, get_default_value c))
            return (a:b:[c])

stmts :: Parsec [Token] [(Token,Token)] [Token]
stmts = do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)

assign :: Parsec [Token] [(Token,Token)] [Token]
assign = do
          a <- idToken
          b <- assignToken
          c <- intToken
          updateState(symtable_update (a, c))
          return (a:b:[c])

remaining_stmts :: Parsec [Token] [(Token,Token)] [Token]
remaining_stmts = (do a <- semiColonToken
                      b <- assign
                      return (a:b)) <|> (return [])

-- funções para a tabela de símbolos

get_default_value :: Token -> Token
get_default_value (Type "int") = Int 0          

symtable_insert :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtable_update :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_update _ [] = fail "variable not found"
symtable_update (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then (id1, v1) : t
                               else (id2, v2) : symtable_update (id1, v1) t

symtable_remove :: (Token,Token) -> [(Token,Token)] -> [(Token,Token)]
symtable_remove _ [] = fail "variable not found"
symtable_remove (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then t
                               else (id2, v2) : symtable_remove (id1, v1) t                               


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> Either ParseError [Token]
parser tokens = runParser program [] "Error message" tokens

main :: IO ()
main = case parser (getTokens "programaV1V2.pe") of
            { Left err -> print err; 
              Right ans -> print ans
            }