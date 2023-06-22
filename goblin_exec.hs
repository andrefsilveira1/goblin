module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class


import System.IO.Unsafe
import System.Environment




data Type = Numeric Int
data Variables = [(String * Type)] -- nome e tipo
data Functions = [(String * [Token])] -- nome e corpo (TODO: descrever protocolo tbm)


-- Nossa memória que será o user state no parsec
data Memory :: [(Variables * Functions)]


-- parsers para os tokens


idToken = tokenPrim show update_pos get_token where
  get_token (Id x p) = Just (Id x p)
  get_token _      = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _         = Nothing

colonToken = tokenPrim show update_pos get_token where
  get_token (Colon p) = Just (Colon p)
  get_token _     = Nothing

intToken = tokenPrim show update_pos get_token where
  get_token (Int x p) = Just (Int x p)
  get_token _       = Nothing

-- typeToken = tokenPrim show update_pos get_token where
--   get_token (Type (x) p) = Just (Type (x) p)
--   get_token _        = Nothing 
    
openParToken = tokenPrim show update_pos get_token where
  get_token (OpenPar p) = Just (OpenPar p)
  get_token _     = Nothing

closeParToken = tokenPrim show update_pos get_token where
  get_token (ClosePar p) = Just (ClosePar p)
  get_token _   = Nothing
  
varsBlockToken = tokenPrim show update_pos get_token where
  get_token (VarsBlock p) = Just (VarsBlock p)
  get_token _   = Nothing

processBlockToken = tokenPrim show update_pos get_token where
  get_token (ProcessBlock p) = Just (ProcessBlock p)
  get_token _   = Nothing

subprogramsBlockToken = tokenPrim show update_pos get_token where
  get_token (SubprogramsBlock p) = Just (SubprogramsBlock p)
  get_token _   = Nothing

openCurlyBracketsToken = tokenPrim show update_pos get_token where
  get_token (OpenCurlyBrackets p) = Just (OpenCurlyBrackets p)
  get_token _   = Nothing

closeCurlyBracketsToken = tokenPrim show update_pos get_token where
  get_token (CloseCurlyBrackets p) = Just (CloseCurlyBrackets p)
  get_token _   = Nothing

equalsToken = tokenPrim show update_pos get_token where
  get_token (Equals p) = Just (Equals p)
  get_token _   = Nothing

numToken = tokenPrim show update_pos get_token where
  get_token (Num x p) = Just (Num x p)
  get_token _   = Nothing

numWithSpecificationToken = tokenPrim show update_pos get_token where
  get_token (NumWithSpecification p) = Just (NumWithSpecification p)
  get_token _   = Nothing

commaToken = tokenPrim show update_pos get_token where
  get_token (Comma p) = Just (Comma p)
  get_token _   = Nothing

addToken = tokenPrim show update_pos get_token where
  get_token (Add p) = Just (Add p)
  get_token _   = Nothing






  

floatToken :: ParsecT [Token] st IO (Token)
floatToken = tokenPrim show update_pos get_token where 
  get_token (Float x p)   = Just (Float x p)
  get_token _           = Nothing

booleanToken :: ParsecT [Token] st IO (Token)
booleanToken = tokenPrim show update_pos get_token where 
  get_token (Boolean x p)  = Just (Boolean x p)
  get_token _            = Nothing

stringToken :: ParsecT [Token] st IO (Token)
stringToken = tokenPrim show update_pos get_token where 
  get_token (String x p)   = Just (String x p)
  get_token _            = Nothing

charToken :: ParsecT [Token] st IO (Token)
charToken = tokenPrim show update_pos get_token where 
  get_token (Char x p)     = Just (Char x p)
  get_token _            = Nothing


update_pos :: SourcePos -> Token -> [Token] -> SourcePos
update_pos pos _ (tok:_) = pos -- necessita melhoria
update_pos pos _ []      = pos  




-- parsers para os não-terminais  
program :: ParsecT [Token] Memory IO ([Token]) -- Memory define o tipo do user state
program = do
            a <- varsBlock
            b <- subprogramsBlock
            c <- processBlock 
            eof
            return (a ++ b ++ c)






-- ------------------------------varsBlock---------------------------

varsBlock :: ParsecT [Token] Memory IO ([Token])
varsBlock = (do 
              a <- varsBlockToken
              b <- colonToken
              c <- varDecls
              return ([a] ++ [b] ++ c)) <|> (return [])

varDecls :: ParsecT [Token] Memory IO ([Token])
varDecls = (do 
              a <- varDecl
              b <- remainingVarDecls
              return (a ++ b))


varDecl :: ParsecT [Token] Memory IO ([Token])
varDecl = do
            a <- typeAndId
            b <- semiColonToken
            s <- getState
            liftIO (print s)
            return (a ++ [b])

remainingVarDecls :: ParsecT [Token] Memory IO ([Token])
remainingVarDecls = (varDecls) <|> (return [])


typeVar :: ParsecT [Token] Memory IO (Token)
typeVar = 
          (numType) 
          -- <|> (othersTypeToken)



numType :: ParsecT [Token] Memory IO (Token)
numType = 
          (numToken) <|> (numWithSpecificationToken)








-- ------------------------------subprogramsBlock---------------------------

subprogramsBlock :: ParsecT [Token] Memory IO ([Token])
subprogramsBlock = (do 
                    a <- subprogramsBlockToken
                    b <- colonToken
                    c <- subPrograms
                    return ([a] ++ [b] ++ c)) <|> (return [])


subPrograms :: ParsecT [Token] Memory IO ([Token])
subPrograms = do 
                a <- subProgram
                b <- remainingSubPrograms
                return (a ++ b)


subProgram :: ParsecT [Token] Memory IO ([Token])
subProgram = do 
                a <- typeVar
                b <- idToken
                c <- openParToken
                d <- parametersList
                e <- closeParToken
                f <- subProgramBody
                return ([a] ++ [b] ++ [c] ++ d ++ [e] ++ f)


parametersList :: ParsecT [Token] Memory IO ([Token])
parametersList = do 
                a <- typeAndId
                b <- remainingParameters
                return (a ++ b)

typeAndId :: ParsecT [Token] Memory IO ([Token])
typeAndId = do 
                a <- typeVar
                b <- idToken
                updateState(symtable_insert (b, get_default_value a))
                return ([a] ++ [b])

remainingParameters :: ParsecT [Token] Memory IO ([Token])
remainingParameters = (do 
                a <- commaToken
                b <- typeAndId
                return ([a] ++ b)) <|> (return [])


remainingSubPrograms :: ParsecT [Token] Memory IO ([Token])
remainingSubPrograms = 
                      (subProgram) <|> (return [])


subProgramBody :: ParsecT [Token] Memory IO ([Token])
subProgramBody = do 
                    a <- openCurlyBracketsToken
                    b <- varsBlock
                    c <- processBlock
                    d <- closeCurlyBracketsToken
                    return ([a] ++ b ++ c ++ [d])








-- ------------------------------processBlock---------------------------

processBlock :: ParsecT [Token] Memory IO ([Token])
processBlock = do 
                  a <- processBlockToken
                  b <- colonToken
                  c <- stmts
                  return ([a] ++ [b] ++ c)


stmts :: ParsecT [Token] Memory IO ([Token])
stmts = do
          first <- stmt
          next <- remainingStmts
          return (first ++ next)


stmt :: ParsecT [Token] Memory IO ([Token])
stmt = do
          a <- assign
          b <- semiColonToken
          return (a ++ [b])

remainingStmts :: ParsecT [Token] Memory IO ([Token])
remainingStmts = 
                (stmts) <|> (return [])

assign :: ParsecT [Token] Memory IO ([Token])
assign = do
          a <- idToken
          b <- equalsToken
          c <- expression
          updateState(symtableUpdate (a, c))
          s <- getState
          liftIO (print s)
          return ([a] ++ [b] ++ [c])

expression :: ParsecT [Token] Memory IO (Token)
expression = try binOp <|> 
                intToken <|> 
                varId 

                -- <|> 
                -- subProgramCall

binOp :: ParsecT [Token] Memory IO (Token)
binOp = do 
          a <- operand
          b <- op
          c <- expression
          return (evalOp a b c)


operand :: ParsecT [Token] Memory IO (Token)
operand = varId <|> intToken

op :: ParsecT [Token] Memory IO (Token)
op = addToken

varId :: ParsecT [Token] Memory IO (Token)
varId = do 
            a <- idToken
            s <- getState
            return (evalVar a s)


                  

-- subProgramCall :: ParsecT [Token] [(Token,Token)] IO ([Token])
-- subProgramCall = do 
--                   a <- idToken
--                   b <- openParToken
--                   c <- argumentList
--                   d <- closeParToken
--                   e <- semiColonToken
--                   return ([a] ++ [b] ++ c ++ [d] ++ [e])


-- argumentList :: ParsecT [Token] [(Token,Token)] IO ([Token])
-- argumentList = do  
--                   a <- idToken
--                   b <- remainingArguments
--                   return ([a] ++ b)


-- remainingArguments :: ParsecT [Token] [(Token,Token)] IO ([Token])
-- remainingArguments = (do  
--                   a <- commaToken
--                   b <- argumentList
--                   return ([a] ++ b)) <|> (return [])










-- funções para a tabela de símbolos

evalOp :: Token -> Token -> Token -> Token
evalOp (Int x p) (Add _) (Int y _) = Int (x + y) p

[(x, 10), (y, 15)]
[([ (x, Numeric 10), (y, Numeric 15) ], [_])]
-- Formato antigo: [(Token, Token)]
evalVar :: Token -> Memory-> Token
evalVar t ((vars, f):lm) = (evalVarAux t vars, f):lm
                              

evalVarAux :: Token -> Variables -> Token
evalVarAux (Id x p) ((name, Numeric v):lv) = 
                                    if x == name then v
                                    else evalVarAux (Id x p) lv

evalVar (Id x p) ((Id id1 _, v1):t) = 
                                if x == id1 then v1
                                else evalVar (Id x p) t

get_default_value :: Token -> Token
get_default_value (Num "num" p) = Int 0 p

symtable_insert :: (Token,Token) -> Memory -> Memory
symtable_insert symbol []  = [symbol]
symtable_insert symbol symtable = symtable ++ [symbol]

symtableUpdate :: (Token, Token) -> Memory -> Memory
symtableUpdate _ [] = fail "variable not found"
symtableUpdate id1 (vars, lf):lm) = (symtableUpdateAux id1 vars, lf):lm

-- TODO
symtableUpdateAux :: (Token, Token) -> Variables -> Memory
symtableUpdateAux (Id id1 p, v1) ((name, Numeric v):lv) = 
                               if id1 == name then (Id id1 p, v1):lv
                               else (id2, v2) : symtableUpdate (id1, v1) t

symtable_remove :: (Token,Token) -> Memory -> Memory
symtable_remove _ [] = fail "variable not found"
symtable_remove (id1, v1) ((id2, v2):t) = 
                               if id1 == id2 then t
                               else (id2, v2) : symtable_remove (id1, v1) t                               

-- parser para memória


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program [] "Error message" tokens

main :: IO ()
main = do a <- getArgs
          case unsafePerformIO (parser (getTokens (a !! 0))) of
            { Left err -> print err; 
              Right ans -> print ans
            }