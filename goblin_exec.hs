module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class

import System.IO.Unsafe

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

program :: ParsecT [Token] [(Token, Token)] IO ([Token])
program = do
            a <- varsBlock
            b <- subprogramsBlock
            c <- processBlock
            eof
            return (a ++ b ++ c)






-- ------------------------------varsBlock---------------------------

varsBlock :: ParsecT [Token] [(Token, Token)] IO ([Token])
varsBlock = (do 
              a <- varsBlockToken
              b <- colonToken
              c <- varDecls
              return ([a] ++ [b] ++ c)) <|> (return [])

varDecls :: ParsecT [Token] [(Token, Token)] IO ([Token])
varDecls = (do 
              a <- varDecl
              b <- remainingVarDecls
              return (a ++ b))


varDecl :: ParsecT [Token] [(Token,Token)] IO ([Token])
varDecl = do
            a <- typeAndId
            b <- semiColonToken
            s <- getState
            liftIO (print s)
            return (a ++ [b])

remainingVarDecls :: ParsecT [Token] [(Token, Token)] IO ([Token])
remainingVarDecls = (varDecls) <|> (return [])


typeVar :: ParsecT [Token] [(Token, Token)] IO (Token)
typeVar = 
          (numType) 
          -- <|> (othersTypeToken)



numType :: ParsecT [Token] [(Token, Token)] IO (Token)
numType = 
          (numToken) <|> (numWithSpecificationToken)








-- ------------------------------subprogramsBlock---------------------------

subprogramsBlock :: ParsecT [Token] [(Token, Token)] IO ([Token])
subprogramsBlock = (do 
                    a <- subprogramsBlockToken
                    b <- colonToken
                    c <- subPrograms
                    return ([a] ++ [b] ++ c)) <|> (return [])


subPrograms :: ParsecT [Token] [(Token, Token)] IO ([Token])
subPrograms = do 
                a <- subProgram
                b <- remainingSubPrograms
                return (a ++ b)


subProgram :: ParsecT [Token] [(Token, Token)] IO ([Token])
subProgram = do 
                a <- typeVar
                b <- idToken
                c <- openParToken
                d <- parametersList
                e <- closeParToken
                f <- subProgramBody
                return ([a] ++ [b] ++ [c] ++ d ++ [e] ++ f)


parametersList :: ParsecT [Token] [(Token, Token)] IO ([Token])
parametersList = do 
                a <- typeAndId
                b <- remainingParameters
                return (a ++ b)

typeAndId :: ParsecT [Token] [(Token, Token)] IO ([Token])
typeAndId = do 
                a <- typeVar
                b <- idToken
                updateState(symtable_insert (b, get_default_value a))
                return ([a] ++ [b])

remainingParameters :: ParsecT [Token] [(Token, Token)] IO ([Token])
remainingParameters = (do 
                a <- commaToken
                b <- typeAndId
                return ([a] ++ b)) <|> (return [])


remainingSubPrograms :: ParsecT [Token] [(Token, Token)] IO ([Token])
remainingSubPrograms = 
                      (subProgram) <|> (return [])


subProgramBody :: ParsecT [Token] [(Token, Token)] IO ([Token])
subProgramBody = do 
                    a <- openCurlyBracketsToken
                    b <- varsBlock
                    c <- processBlock
                    d <- closeCurlyBracketsToken
                    return ([a] ++ b ++ c ++ [d])








-- ------------------------------processBlock---------------------------

processBlock :: ParsecT [Token] [(Token, Token)] IO ([Token])
processBlock = do 
                  a <- processBlockToken
                  b <- colonToken
                  c <- stmts
                  return ([a] ++ [b] ++ c)


stmts :: ParsecT [Token] [(Token,Token)] IO ([Token])
stmts = do
          first <- stmt
          next <- remainingStmts
          return (first ++ next)


stmt :: ParsecT [Token] [(Token,Token)] IO ([Token])
stmt = do
          a <- assign
          b <- semiColonToken
          return (a ++ [b])

remainingStmts :: ParsecT [Token] [(Token,Token)] IO ([Token])
remainingStmts = 
                (stmts) <|> (return [])

assign :: ParsecT [Token] [(Token,Token)] IO ([Token])
assign = do
          a <- idToken
          b <- equalsToken
          c <- expression
          updateState(symtable_update (a, c))
          s <- getState
          liftIO (print s)
          return ([a] ++ [b] ++ [c])

expression :: ParsecT [Token] [(Token,Token)] IO (Token)
expression = try binOp <|> 
                intToken <|> 
                varId 

                -- <|> 
                -- subProgramCall

binOp :: ParsecT [Token] [(Token,Token)] IO (Token)
binOp = do 
          a <- operand
          b <- op
          c <- expression
          return (evalOp a b c)


operand :: ParsecT [Token] [(Token,Token)] IO (Token)
operand = varId <|> intToken

op :: ParsecT [Token] [(Token,Token)] IO (Token)
op = addToken

varId :: ParsecT [Token] [(Token,Token)] IO (Token)
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

evalVar :: Token -> [(Token, Token)]-> Token
evalVar (Id x p) ((Id id1 _, v1):t) = 
                                if x == id1 then v1
                                else evalVar (Id x p) t

get_default_value :: Token -> Token
get_default_value (Num "num" p) = Int 0 p

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
main = case unsafePerformIO (parser (getTokens "program-readable.pe")) of
            { Left err -> print err; 
              Right ans -> print ans
            }