module Main (main) where

import Lexer
import Text.Parsec
import Control.Monad.IO.Class
import Control.Monad


import System.IO.Unsafe
import System.Environment



--type Digit = 0 | 1 | 2...| 9
data Type = Numeric Int | Boolean Bool-- | NumericWithSpec ((Int, [Digit]), (Int, [Digit]))
type Variable = (String, Type) -- nome e tipo
type Variables = [Variable] -- nome e tipo

type FunName = String
type FunParams = [Token]
type FunBody = [Token]

--TODO Refatorar estrutura de Function no código (arguments foram removidos)
type Function = (FunName, FunParams, FunBody)

type Functions = [Function] 
type Stack = [(Variables, Functions)]

-- TODO: Atualizar todos os cantos que usam memória
-- Nossa memória que será o user state no parsec
type Memory = (Stack, Bool, Bool) -- stack de variáveis e funções e flag indicativa de execução


getTopFuns :: Stack -> Functions
getTopFuns (t:_) = getFunsFromStackCell t
getTopFuns [] = []

getFunsFromStackCell :: (Variables, Functions) -> Functions
getFunsFromStackCell (_, funs) = funs


getTopVars :: Stack -> Variables
getTopVars (t:_) = getVarsFromStackCell t
getTopVars [] = []

getVarsFromStackCell :: (Variables, Functions) -> Variables
getVarsFromStackCell (vars, _) = vars

isRunning :: Memory -> Bool
isRunning (_, ie, _) = ie

isBlockRunning :: Memory -> Bool
isBlockRunning (_, _, irb) = irb 


beginExecution :: Memory -> Memory
beginExecution (stack, _ , _) = (stack, True, True)

beginBlockExecution :: Memory -> Memory
beginBlockExecution (stack, fe, _) = (stack, True, True)

endBlockExecution :: Memory -> Memory
endBlockExecution (stack, fe, _) = (stack, fe, False)










printMem :: Memory -> IO ()
printMem (stack, _, _) = print (printMemVars (getTopVars stack) ++ "FUNS: " ++ printMemFuns (getTopFuns stack))

printMemVars :: Variables -> String
printMemVars  [] = []
printMemVars ((name, Numeric val):lv) = name ++ " " ++ show val ++ ", " ++ printMemVars lv


printMemFuns :: Functions -> String
printMemFuns  [] = []
printMemFuns ((name, _, _):lf) = name ++ ", "  ++ printMemFuns lf

pushMemStack :: Memory -> Memory
pushMemStack (stack:sl, ir, irb) = ([stack, stack] ++ sl, ir, irb)

popMemStack :: Memory -> Memory
popMemStack (stack:sl, ir, irb) = (sl, ir, irb)


auxPrint :: Type -> String
auxPrint (Numeric value) = show value
--auxPrint (StringLit value _) = init(tail(value))

printVars :: Token -> Type -> IO ()
printVars (StringLit value _) tok = putStrLn (init(tail(value)) ++ auxPrint(tok))
-- ++ " " ++ show tok ++ "\n"



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

subToken = tokenPrim show update_pos get_token where
  get_token (Sub p) = Just (Sub p)
  get_token _   = Nothing

multToken = tokenPrim show update_pos get_token where
  get_token (Mult p) = Just (Mult p)
  get_token _   = Nothing

powToken = tokenPrim show update_pos get_token where
  get_token (Pow p) = Just (Pow p)
  get_token _   = Nothing

divToken = tokenPrim show update_pos get_token where
  get_token (Div p) = Just (Div p)
  get_token _   = Nothing

printToken = tokenPrim show update_pos get_token where
  get_token (Print p) = Just (Print p)
  get_token _    = Nothing

returnToken = tokenPrim show update_pos get_token where
  get_token (Return p) = Just (Return p)
  get_token _   = Nothing

ifToken = tokenPrim show update_pos get_token where
  get_token (If p) = Just (If p)
  get_token _    = Nothing

elseToken = tokenPrim show update_pos get_token where
  get_token (Else p) = Just (Else p)
  get_token _ = Nothing

-- elseifToken = tokenPrim show update_pos get_token where
--   get_token (ElseIf p) = Just (ElseIf p)
--   get_token _ = Nothing  

lessToken = tokenPrim show update_pos get_token where
  get_token (Less p) = Just (Less p)
  get_token _    = Nothing

greaterToken = tokenPrim show update_pos get_token where
  get_token (Greater p) = Just (Greater p)
  get_token _    = Nothing







  

floatToken :: ParsecT [Token] st IO (Token)
floatToken = tokenPrim show update_pos get_token where 
  get_token (Float x p)   = Just (Float x p)
  get_token _           = Nothing

-- booleanToken :: ParsecT [Token] st IO (Token)
-- booleanToken = tokenPrim show update_pos get_token where 
--   get_token (Boolean x p)  = Just (Boolean x p)
--   get_token _            = Nothing

stringLitToken :: ParsecT [Token] st IO (Token)
stringLitToken = tokenPrim show update_pos get_token where 
  get_token (StringLit x p)   = Just (StringLit x p)
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
            updateState(beginExecution)
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
            liftIO (printMem s)
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
                updateState(pushMemStack)
                c <- openParToken
                d <- parametersList
                e <- closeParToken
                s <- getState
                -- when (isRunning s) ()
                (f, _) <- subProgramBody
                updateState(popMemStack)
                s <- getState
                let allTokens = [a] ++ [b] ++ [c] ++ d ++ [e] ++ f
                updateState(symtable_insert_subprogram b d f)
                return (allTokens)


parametersList :: ParsecT [Token] Memory IO ([Token])
parametersList = do 
                a <- typeAndId
                b <- remainingParameters
                return (a ++ b)

typeAndId :: ParsecT [Token] Memory IO ([Token])
typeAndId = do 
                a <- typeVar
                b <- idToken
                c <- getState
                updateState(symtable_insert_var b (get_default_value a))
                return ([a] ++ [b])

remainingParameters :: ParsecT [Token] Memory IO ([Token])
remainingParameters = (do 
                a <- commaToken
                b <- typeAndId
                return ([a] ++ b)) <|> (return [])


remainingSubPrograms :: ParsecT [Token] Memory IO ([Token])
remainingSubPrograms = 
                      (subProgram) <|> (return [])


subProgramBody :: ParsecT [Token] Memory IO ([Token], Type)
subProgramBody = do 
                    a <- openCurlyBracketsToken
                    b <- varsBlock
                    c <- processBlock
                    d <- returnToken
                    (expT, expVal) <- expression
                    e <- semiColonToken
                    f <- closeCurlyBracketsToken
                    return ([a] ++ b ++ c ++ [d] ++ expT ++ [e] ++ [f], expVal)








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
          a <- (assign <|> printVar <|> ifMainBlock)
          b <- semiColonToken
          return (a ++ [b])

remainingStmts :: ParsecT [Token] Memory IO ([Token])
remainingStmts = 
                (stmts) <|> (return [])

-------Print---------

printVar :: ParsecT [Token] Memory IO ([Token])
printVar = do 
                a <- printToken
                b <- openParToken
                string <- stringLitToken
                comma <- commaToken
                (expT, expVal) <- expression
                g <- closeParToken
                liftIO (printVars string expVal)
                return ([a] ++ [b]++ [string] ++  [comma] ++ expT ++ [g])




-- ------------------------------Salto condicional---------------------------



ifMainBlock :: ParsecT [Token] Memory IO ([Token])
ifMainBlock = do
                (_, (Boolean valor)) <- ifEvalue
                s <- getState
                when(isRunning s && not valor) (updateState(endBlockExecution))
                a <- block
                when(isRunning s && not valor) (updateState(beginBlockExecution))
                b <- remainingIfblock
                updateState(beginBlockExecution)
                return (a ++ b)

ifEvalue :: ParsecT [Token] Memory IO ([Token], Type)
ifEvalue = (do
            a <- ifToken
            b <- openParToken
            (c, valor) <- expression
            d <- closeParToken
            return ([a] ++ [b] ++ c ++ [d], valor))

remainingIfblock :: ParsecT [Token] Memory IO ([Token])
remainingIfblock = try (
                          do 
                          a <- elseToken
                          (b, (Boolean valor)) <- ifEvalue
                          s <- getState
                          when(isRunning s && not valor) (updateState(endBlockExecution))
                          c <- block
                          when(isRunning s && not valor) (updateState(beginBlockExecution))
                          d <- remainingIfblock
                          return ([a] ++ b ++ c ++ d)
                        )
                     <|>
                        (
                          do
                          a <- elseToken
                          b <- block
                          return ([a] ++ b)
                        )
                     <|> return ([]) 


block :: ParsecT [Token] Memory IO ([Token])
block = do
            e <- openCurlyBracketsToken
            f <- stmts
            g <- closeCurlyBracketsToken
            return ([e] ++ f ++ [g])



canExecute :: ParsecT [Token] Memory IO (Bool) 
canExecute = do
                s <- getState
                return (isRunning s && isBlockRunning s)



assign :: ParsecT [Token] Memory IO ([Token])
assign = do
          a <- idToken
          b <- equalsToken
          (expT, expVal) <- expression
          ce <- canExecute
          when (ce) (updateState(symtableUpdate a expVal))
          e <- getState
          liftIO (printMem e)
          return ([a] ++ [b] ++ expT)


expression :: ParsecT [Token] Memory IO ([Token], Type)
expression = try binOp <|> unaryExpression

unaryExpression :: ParsecT [Token] Memory IO ([Token], Type)
unaryExpression = do 
                    a <- op
                    (tok, val) <- intLit
                    let valWithSign = evalSign a val
                    return ([a] ++ tok, valWithSign)


-- TODO: finalizar implemnetação
evalSign :: Token -> Type -> Type
evalSign _ t = t


--- funções considerando associatividade à esquerda 
binOp :: ParsecT [Token] Memory IO ([Token], Type)
binOp = do
  (operandT, operandV) <- operand
  (tok, val) <- evalueRemaining (operandV)
  return (operandT ++ tok, val)


evalueRemaining :: Type -> ParsecT [Token] Memory IO ([Token], Type)
evalueRemaining numb = do
                      a <- op
                      (operandT, operandV) <- operand
                      (tok, val) <- evalueRemaining (evalOp numb a operandV)
                      return ([a] ++ operandT ++ tok, val)
                    <|> return ([], numb)

operand :: ParsecT [Token] Memory IO ([Token], Type)
operand = try subProgramCall <|>
          (varId <|>
          intLit)

intLit :: ParsecT [Token] Memory IO ([Token], Type)
intLit = do
            (Int v p) <- intToken
            return ([(Int v p)], Numeric v)

op :: ParsecT [Token] Memory IO (Token)
op = powToken <|> multToken <|> divToken <|> addToken <|> subToken <|> lessToken <|> greaterToken

varId :: ParsecT [Token] Memory IO ([Token], Type)
varId = do 
            a <- idToken
            s <- getState
            return ([a], evalVar a s)




                  

subProgramCall :: ParsecT [Token] Memory IO ([Token], Type)
subProgramCall = do 
                  (Id funName p) <- idToken
                  b <- openParToken
                  c <- argumentList
                  d <- closeParToken
                  updateState(pushMemStack)
                  updateState(addParametersToMemory (Id funName p) c)

                  (s, _, _) <- getState
                  let (_, _, funBody) = findFun funName (getTopFuns s)
                  inp <- getInput
                  setInput funBody
                  (_, v) <- subProgramBody
                  setInput inp

                  updateState(popMemStack)
                  return ([(Id funName p)] ++ [b] ++ c ++ [d], v)


argumentList :: ParsecT [Token] Memory IO ([Token])
argumentList = do  
                  a <- idToken
                  b <- remainingArguments
                  return ([a] ++ b)

remainingArguments :: ParsecT [Token] Memory IO ([Token])
remainingArguments = (do  
                  a <- commaToken
                  b <- argumentList
                  return ([a] ++ b)) <|> (return [])









-- funções para a tabela de símbolos

evalOp :: Type -> Token -> Type -> Type
evalOp (Numeric x) (Pow _) (Numeric y) = Numeric (x ^ y)
evalOp (Numeric x) (Mult _) (Numeric y) = Numeric (x * y)
evalOp (Numeric x) (Div _) (Numeric y) = Numeric (x `div` y)
evalOp (Numeric x) (Add _) (Numeric y) = Numeric (x + y)
evalOp (Numeric x) (Sub _) (Numeric y) = Numeric (x - y)
evalOp (Numeric x) (Greater _) (Numeric y) = Boolean (x > y)
evalOp (Numeric x) (Less _) (Numeric y) = Boolean (x < y)

-- [(x, 10), (y, 15)]
-- [([ (x, Numeric 10), (y, Numeric 15) ], [_])]
-- Formato antigo: [(Token, Token)]


evalVar :: Token -> Memory -> Type
evalVar t (stack, _, _) = evalVarAux t (getTopVars stack)
                              

-- TODO: Por que fail não é aceito pelo compilador nessa função?
evalVarAux :: Token -> Variables -> Type
evalVarAux (Id x (l, c)) [] = error ("variable " ++ x ++ " not in scope at line " ++ show l ++ ", column " ++ show c)
evalVarAux (Id x p) ((name, Numeric v):lv) = 
                                    if x == name then Numeric v
                                    else evalVarAux (Id x p) lv

--evalFunCall :: Token -> Memory -> ParsecT [Token] Memory IO (Type)
--evalFunCall (Id funName _) ((_, funs):_, _) = do
--                                                 a <- getInput
--                                                 b <- getState
--                                                 setInput funBody
--                                                 x <- runParserT subProgramBody b "Error message" funBody
--                                                 return (v)
--                                                   where (_, _, funBody) = findFun funName funs
--evalFunCall :: Token -> Type
--evalFunCall _ = Numeric 0

-- TODO: Exibir erro caso não encontre função
findFun :: String -> Functions -> Function
findFun name ((n, params, body):lf) = if name == n then (n, params, body)
                                           else findFun name lf

addParametersToMemory :: Token -> [Token] -> Memory -> Memory
addParametersToMemory (Id name _) args ((vars, funs):lm , ir, irb) = addParametersToMemoryAux args params ((vars, funs):lm , ir, irb)
    where (_, params, _) = findFun name funs

addParametersToMemoryAux :: [Token] -> [Token] -> Memory -> Memory
addParametersToMemoryAux ((Id name _):args) ((Id paramName pp):params) mem =
    addParametersToMemoryAux args params updatedMem
        where
            (_, val) = getVar name mem
            updatedMem = symtable_insert_var (Id paramName pp) val mem

-- In case of a collon or type in the paramList
addParametersToMemoryAux args ((Comma _):params) mem = addParametersToMemoryAux args params mem
addParametersToMemoryAux args ((Num _ _):params) mem = addParametersToMemoryAux args params mem
addParametersToMemoryAux ((Comma _):args) params mem = addParametersToMemoryAux args params mem
addParametersToMemoryAux [] [] mem = mem


--getVarVal :: Variable -> Type
--getVarVal (_, val) = val

-- Get a variable by name from the most recent reference enviroment of the Stack
getVar :: String -> Memory -> Variable
getVar name (st:ls, _, _) = getVarAux name (getVarsFromStackCell st)

getVarAux :: String -> Variables -> Variable
getVarAux srcName ((tgtName, varType):lv) =
    if srcName == tgtName then (tgtName, varType)
    else getVarAux srcName lv




get_default_value :: Token -> Type
get_default_value (Num "num" _) = Numeric 0


symtable_insert_var :: Token -> Type -> Memory -> Memory
symtable_insert_var (Id name _) varType ([], ir, irb) = ((updatedVars, []):[], ir, irb)
                                                    where updatedVars = addVarToMemory name varType []
symtable_insert_var (Id name _) varType (s:ls, ir, irb) = ((updatedVars, (getFunsFromStackCell s)):ls, ir, irb)
                                                    where updatedVars = addVarToMemory name varType (getVarsFromStackCell s)

addVarToMemory :: String -> Type -> Variables -> Variables
addVarToMemory name varType vars = (name, varType):vars

symtable_insert_subprogram :: Token -> [Token] -> [Token] -> Memory -> Memory
symtable_insert_subprogram (Id name _) parametersList allTokens ([], ir, irb) = (([], updatedFunctions):[], ir, irb)
    where updatedFunctions = addSubprogramToMemory name parametersList allTokens []
symtable_insert_subprogram (Id name _) parametersList allTokens (s:ls, ir, irb) = ((getVarsFromStackCell s, updatedFunctions):ls, ir, irb)
    where updatedFunctions = addSubprogramToMemory name parametersList allTokens (getFunsFromStackCell s)

addSubprogramToMemory :: String -> [Token] -> [Token] -> Functions -> Functions
addSubprogramToMemory name parametersList body funs = (name, parametersList, body):funs

-- TODO: Por que fail não é aceito pelo compilador nessa função após o isRunning ser adicionado a memória?
-- Recebe um idToken, um typeToken, a memória e retorna a memória atualizada
symtableUpdate :: Token -> Type -> Memory -> Memory
symtableUpdate idT val (s:ls, ir, irb) = ((symtableUpdateAux idT val (getVarsFromStackCell s), getFunsFromStackCell s):ls, ir, irb)

symtableUpdateAux :: Token -> Type -> Variables -> Variables
symtableUpdateAux _ _ [] = fail "variable not found"
symtableUpdateAux (Id id1 p) val ((name, Numeric v):lv) =
                               if id1 == name then (id1, val):lv
                               else (name, Numeric v) : symtableUpdateAux (Id id1 p) val lv


-- symtable_remove :: (Token,Token) -> Memory -> Memory
-- symtable_remove _ [] = fail "variable not found"
-- symtable_remove (id1, v1) ((id2, v2):t) = 
--                                if id1 == id2 then t
--                                else (id2, v2) : symtable_remove (id1, v1) t                               

-- parser para memória


-- invocação do parser para o símbolo de partida 

parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program ([], False, False) "Error message" tokens

main :: IO ()
main = do a <- getArgs
          case unsafePerformIO (parser (getTokens (a !! 0))) of
            { Left err -> print err; 
              Right ans -> print ans
            }
