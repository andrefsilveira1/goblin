module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe
import Text.Parsec (ParsecT, eof)
import GHC (Token)
import Data.Binary.Get (remaining)


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

semiColonToken :: ParsecT [Token] st IO (Token)
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

functionToken = tokenPrim show update_pos get_token where
  get_token Function = Just Function 
  get_token _        = Nothing
    
openToken = tokenPrim show update_pos get_token where
  get_token Begin = Just Begin
  get_token _     = Nothing

fparams = tokenPrim show update_pos get_token where
  get_token Param = Just Param
  get_token _     = Nothing
  
closeToken = tokenPrim show update_pos get_token where
  get_token End = Just End
  get_token _   = Nothing

typeToken = tokenPrim show update_pos get_token where
  get_token (Type x) = Just (Type x)
  get_token _        = Nothing

subProgram = tokenPrim show update_pos get_token where
  get_token Program = Just subProgram
  get_token _       = Nothing
  
remainingSubPrograms = tokenPrim show update_pos get_token where
  get_token Program = Just Program
  get_token _       = Nothing


update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  

-- parsers para os não-terminais
-- <function> -> FUNCAO ID AP <fparams> FP DP TYPE BEGIN <stmts> END
-- function = do 
--              a <- funcaoToken
--              b <- idToken
--              |
--              eof
--              return (a:b:[c] ++ d ++ [e] ++ f ++ [g])

program :: ParsecT [Token] [(Token, Token)] IO ([Token])
program = do
            a <- subPrograms
            b <- principal
            eof
            return (a ++ b)
subPrograms :: ParsecT [Token] [(Token, Token)] IO ([Token])
subPrograms = do 
                a <- subProgram
                b <- remainingSubPrograms
                return (a ++ b)

remainingSubPrograms :: ParsecT [Token] [(Token, Token)] IO ([Token])
remainingSubPrograms =
                      (subPrograms) <|> (return [])  

subProgram :: ParsecT [Token] [(Token, Token)] IO ([Token])
subProgram = do
               a <- function
               return (a)
                         
principal :: ParsecT [Token] [(Token,Token)] IO ([Token])
principal = do
            a <- programToken 
            b <- idToken 
            c <- varToken
            d <- varDecl
            e <- beginToken 
            f <- stmts
            g <- endToken
            return (a:b:[c] ++ d++ [e] ++ f ++ [g])
function :: ParsecT [Token] [(Token,Token)] IO [(Token)]
function = do 
             a <- functionToken
             b <- idToken
             c <- openToken
             d <- fparams
             e <- closeToken
             f <- typeToken
             g <- beginToken
             h <- stmts
             i <- endToken
             return ([a] ++ [b] ++ [c]:d ++ [e] ++ [f] ++ [g]:h ++ [i] )

varDecl :: ParsecT [Token] [(Token,Token)] IO([Token])
varDecl = do
            a <- idToken
            b <- colonToken
            c <- typeToken
            updateState(symtable_insert (a, get_default_value c))
            s <- getState
            liftIO (print s)
            return (a:b:[c])

stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = do
          first <- assign
          next <- remaining_stmts
          return (first ++ next)

assign :: ParsecT [Token] [(Token,Token)] IO([Token])
assign = do
          a <- idToken
          b <- assignToken
          c <- intToken
          updateState(symtable_update (a, c))
          s <- getState
          liftIO (print s)
          return (a:b:[c])

remaining_stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
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

-- parser para memória


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = case unsafePerformIO (parser (getTokens "programaV1V2.pe")) of
            { Left err -> print err; 
              Right ans -> print ans
            }