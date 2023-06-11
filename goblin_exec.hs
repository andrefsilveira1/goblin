module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

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
    
openParToken = tokenPrim show update_pos get_token where
  get_token OpenPar = Just OpenPar
  get_token _     = Nothing

closeToken = tokenPrim show update_pos get_token where
  get_token ClosePar = Just ClosePar
  get_token _   = Nothing
  
floatToken :: ParsecT [Token] st IO (Token)
floatToken = tokenPrim show update_pos get_token where 
  get_token (Float x)   = Just (Float x)
  get_token _           = Nothing

booleanToken :: ParsecT [Token] st IO (Token)
booleanToken = tokenPrim show update_pos get_token where 
  get_token (Boolean x)  = Just (Boolean x)
  get_token _            = Nothing

stringToken :: ParsecT [Token] st IO (Token)
stringToken = tokenPrim show update_pos get_token where 
  get_token (String x)   = Just (String x)
  get_token _            = Nothing

charToken :: ParsecT [Token] st IO (Token)
charToken = tokenPrim show update_pos get_token where 
  get_token (Char x)     = Just (Char x)
  get_token _            = Nothing

  -- intToken = tokenPrim show update_pos get_token where
  -- get_token (Int x) = Just (Int x)
  -- get_token _       = Nothing

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
-- <fparams> -> typeToken idToken 

program :: ParsecT [Token] [(Token, Token)] IO ([Token])
program = do
            a <- varsBlock
            b <- subprogramBlock
            c <- processBlock
            eof
            return (a ++ b ++ c)

-- TODO: varsBlockToken
varsBlock :: ParsecT [Token] [(Token, Token)] IO ([Token])
varsBlock = (do 
              a <- varsBlockToken
              b <- colonToken
              c <- varDecls
              return ([a] ++ b ++ c)) <|> (return [])

varDecls :: ParsecT [Token] [(Token, Token)] IO ([Token])
varDecls = (do 
              a <- varDecl
              b <- remainingVarDecls
              return (a ++ b))

-- TODO: semicolonToken
varDecl :: ParsecT [Token] [(Token,Token)] IO([Token])
varDecl = do
            a <- typeVar
            b <- idToken
            c <- semicolonToken
            updateState(symtable_insert (b, get_default_value a))
            s <- getState
            liftIO (print s)
            return ([a]:[b]:[c])

remainingVarDecls :: ParsecT [Token] [(Token, Token)] IO ([Token])
remainingVarDecls = 
                    (varDecl) <|> (return [])

-- TODO: Checar se retorna lista ou não
typeVar :: ParsecT [Token] [(Token, Token)] IO ([Token])
typeVar = 
          (numType) <|> (othersTypeToken)


-- TODO: numSpecifier, numToken
numType :: ParsecT [Token] [(Token, Token)] IO ([Token])
numType = 
          (numToken) <|>
          (do 
              a <- numToken
              b <- numSpecifier
              return [a] ++ numSpecifier)


-- TODO: subprogramsBlockToken
suprogramsBlock :: ParsecT [Token] [(Token, Token)] IO ([Token])
suprogramsBlock = (do 
                    a <- subprogramsBlockToken
                    b <- colonToken
                    c <- subPrograms
                    return [a] ++ [b] ++ c) <|> (return [])


subPrograms :: ParsecT [Token] [(Token, Token)] IO ([Token])
subPrograms = do 
                a <- subProgram
                b <- remainingSubPrograms
                return a ++ b


subProgram :: ParsecT [Token] [(Token, Token)] IO ([Token])
subProgram = do 
                a <- typeVar
                b <- idToken
                c <- openParToken
                d <- parametersList
                e <- closeParToken
                f <- subProgramBody
                return [a] ++ [b] ++ [c] ++ d ++ [e] ++ f


remainingSubPrograms :: ParsecT [Token] [(Token, Token)] IO ([Token])
remainingSubPrograms = 
                      (subProgram) <|> (return [])


-- TODO: openCurlyBracketsToken, closeCurlyBracketsToken
subProgramBody :: ParsecT [Token] [(Token, Token)] IO ([Token])
subProgramBody = do 
                    a <- openCurlyBracketsToken
                    b <- varsBlock
                    c <- processBlock
                    d <- closeCurlyBracketsToken
                    return [a] ++ b ++ c ++ [d]

-- TODO: processBlockToken
processBlock :: ParsecT [Token] [(Token, Token)] IO ([Token])
processBlock = do 
                  a <- processBlockToken
                  b <- colonToken
                  c <- stmts
                  return [a] ++ [b] ++ c


stmts :: ParsecT [Token] [(Token,Token)] IO([Token])
stmts = do
          first <- stmt
          next <- remaining_stmts
          return (first ++ next)


stmt :: ParsecT [Token] [(Token,Token)] IO([Token])
stmt = do
          a <- assign
          b <- semiColonToken
          return (a ++ b)


-- TODO: equalToken, expression
assign :: ParsecT [Token] [(Token,Token)] IO([Token])
assign = do
          a <- idToken
          b <- equalToken
          c <- expression
          updateState(symtable_update (a, c))
          s <- getState
          liftIO (print s)
          return (a:b:[c])


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
main = case unsafePerformIO (parser (getTokens "program.pe")) of
            { Left err -> print err; 
              Right ans -> print ans
            }