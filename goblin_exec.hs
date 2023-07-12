module Main (main) where

import Control.Monad.IO.Class
import Control.Monad

import System.IO.Unsafe
import System.Environment

import Lexer
import Text.Parsec
import PrimitiveTokens

-----------------------------Memory-----------------------------

--type Digit = 0 | 1 | 2...| 9
data Type = Numeric Int | Boolean Bool | UserDefined (String, Variables, Functions) | NoValue -- | NumericWithSpec ((Int, [Digit]), (Int, [Digit]))
instance Eq Type where
    (Numeric _)  == (Numeric _) = True
    (Boolean _) == (Boolean _) = True
    (UserDefined (name1, _, _)) == (UserDefined (name2, _, _)) = name1 == name2
    NoValue == NoValue = True
    _ == _ = False

  

type Variable = (String, Type) -- nome e tipo
type Variables = [Variable] -- nome e tipo

type FunName = String
type FunParams = [Token]
type FunBody = [Token]
type Function = (FunName, FunParams, FunBody)
type Functions = [Function]

type Stack = [Variables]
type UserTypes = [Type]
type Globals = (Variables, Functions, UserTypes)

-- User state
-- Global variables and functions, stack of local variables, execution flag and block execution flag
type Memory = (Globals, Stack, Bool, Bool)




-----------------------------Memory access functions-----------------------------
getFuns :: Memory -> Functions
getFuns ((_, funs, _), _, _, _) = funs

getUserTypes :: Memory -> UserTypes
getUserTypes ((_, _, uts), _, _, _) = uts


getCurrentVars :: Memory -> Variables
getCurrentVars ((vars, _, _), (sv:_), _, _)  = vars ++ sv
getCurrentVars ((vars, _, _), [], _, _) = vars


-- Get a variable by name from the most recent reference environment of the Stack
getVar :: String -> Memory -> Variable
getVar name mem = getVarAux name (getCurrentVars mem)

getVarAux :: String -> Variables -> Variable
getVarAux srcName ((tgtName, varType):lv) =
    if srcName == tgtName then (tgtName, varType)
    else getVarAux srcName lv
getVarAux srcName [] = error ("variable " ++ srcName ++ " not found")



getVarsFromStackCell :: (Variables, Functions) -> Variables
getVarsFromStackCell (vars, _) = vars

isRunning :: Memory -> Bool
isRunning (_, _, ie, _) = ie

isBlockRunning :: Memory -> Bool
isBlockRunning (_, _, _, irb) = irb






beginExecution :: Memory -> Memory
beginExecution (g, stack, _ , _) = (g, stack, True, True)

beginBlockExecution :: Memory -> Memory
beginBlockExecution (g, stack, fe, _) = (g, stack, True, True)

endBlockExecution :: Memory -> Memory
endBlockExecution (g, stack, fe, _) = (g, stack, fe, False)



---------- Printing Memory ------------
printMem :: Memory -> IO ()
printMem mem = print (printMemVars (getCurrentVars mem) ++ "FUNS: " ++ printMemFuns (getFuns mem))

printMemVars :: Variables -> String
printMemVars  [] = []
printMemVars ((name, Numeric val):lv) = name ++ " " ++ show val ++ ", " ++ printMemVars lv
printMemVars ((name, (UserDefined (utName, fields, funs))):lv) = name ++ "[" ++ utName ++ "]{ fields: " ++ (printMemVars fields) ++ "}, " ++ printMemVars lv
printMemVars ((name, NoValue):lv) = name ++ " " ++ "Fatal Error!!!" ++ ", " ++ printMemVars lv


printMemFuns :: Functions -> String
printMemFuns  [] = []
printMemFuns ((name, _, _):lf) = name ++ ", "  ++ printMemFuns lf

pushMemStack :: Memory -> Memory
pushMemStack (g, stack, ir, irb) = (g, []:stack, ir, irb)

popMemStack :: Memory -> Memory
popMemStack (g, stack:sl, ir, irb) = (g, sl, ir, irb)


auxPrint :: Type -> String
auxPrint (Numeric value) = show value
--auxPrint (StringLit value _) = init(tail(value))

printVars :: Token -> Type -> IO ()
printVars (StringLit value _) tok = putStrLn (init(tail(value)) ++ auxPrint(tok))
-- ++ " " ++ show tok ++ "\n"








-------------------------------Parsers-------------------------------
program :: ParsecT [Token] Memory IO ([Token]) -- Memory define o tipo do user state
program = do
            a <- typesBlock
            b <- varsBlock True
            c <- subprogramsBlock
            updateState(beginExecution)
            (d, _) <- processBlock
            eof
            return (a ++ b ++ c ++ d)







-------------------------------TypesBlock-------------------------------
typesBlock :: ParsecT [Token] Memory IO ([Token])
typesBlock = (do
              a <- typesBlockToken
              b <- colonToken
              c <- typeDecls
              return ([a] ++ [b] ++ c)) <|> (return [])


typeDecls :: ParsecT [Token] Memory IO ([Token])
typeDecls = (do
              a <- typeDecl
              b <- remainingTypeDecls
              return (a ++ b))

typeDecl :: ParsecT [Token] Memory IO ([Token])
typeDecl = do
            a <- idToken
            updateState(insertUserType a)
            b <- openCurlyBracketsToken
            (c, vars) <- fieldsBlock
            updateState(updateUserTypeVars a vars)
            (d, funs) <- operationsBlock
            updateState(updateUserTypeFuns a funs)
            z <- closeCurlyBracketsToken
            return ([a] ++ [b] ++ c ++ d)

remainingTypeDecls :: ParsecT [Token] Memory IO ([Token])
remainingTypeDecls = (typeDecls) <|> (return [])


fieldsBlock :: ParsecT [Token] Memory IO ([Token], Variables)
fieldsBlock = do
                a <- fieldsBlockToken
                b <- colonToken
                (c, fieldVars) <- fields
                return ([a] ++ [b] ++ c, fieldVars)

fields :: ParsecT [Token] Memory IO ([Token], Variables)
fields = do
            (a, fieldVar) <- field
            (b, fieldVars) <- remainingFields
            return (a ++ b, fieldVar:fieldVars)

field :: ParsecT [Token] Memory IO ([Token], Variable)
field = do
           a <- typeVar
           (Id fieldName p) <- idToken
           c <- semiColonToken
           s <- getState
           return ([a] ++ [(Id fieldName p)] ++ [c], (fieldName,  (get_default_value a s)))

remainingFields :: ParsecT [Token] Memory IO ([Token], Variables)
remainingFields = (fields) <|> (return ([], []))


operationsBlock :: ParsecT [Token] Memory IO ([Token], Functions)
operationsBlock = do
               a <- operationsBlockToken
               b <- colonToken
               (c, operationFunctions) <- operations
               return ([a] ++ [b] ++ c, operationFunctions)

operations :: ParsecT [Token] Memory IO ([Token], Functions)
operations = do
            (a, operationFun) <- operation
            (b, operationFuns) <- remainingOperations
            return (a ++ b, operationFun:operationFuns)

remainingOperations :: ParsecT [Token] Memory IO ([Token], Functions)
remainingOperations = (operations) <|> (return ([], []))

operation :: ParsecT [Token] Memory IO ([Token], Function)
operation = do
              a <- typeVar
              (b, name) <- operationName
              updateState(pushMemStack)
              c <- openParToken
              d <- parametersList
              e <- closeParToken
              (f, _) <- subProgramBody
              updateState(popMemStack)
              let allTokens = [a] ++ [b] ++ [c] ++ d ++ [e] ++ f
              return (allTokens, (name, d, f))

operationName :: ParsecT [Token] Memory IO (Token, String)
operationName = (do
                  a <- op
                  return (a, opToSymbol a)) <|>
                (do
                  (Id name p) <- idToken
                  return((Id name p), name))

opToSymbol :: Token -> String
opToSymbol (Pow _) = "^"
opToSymbol (Mult _) = "*"
opToSymbol (Add _) = "+"
opToSymbol (Sub _) = "-"
opToSymbol (Div _) = "/"
opToSymbol (Mod _) = "%"
opToSymbol (Equiv _) = "=="
opToSymbol (Diff _) = "!="

















-------------------------------varsBlock-------------------------------

varsBlock :: Bool -> ParsecT [Token] Memory IO ([Token])
varsBlock global = (do
              a <- varsBlockToken
              b <- colonToken
              c <- varDecls global
              return ([a] ++ [b] ++ c)) <|> (return [])

varDecls :: Bool -> ParsecT [Token] Memory IO ([Token])
varDecls global = (do
              a <- varDecl global
              b <- remainingVarDecls global
              return (a ++ b))


varDecl :: Bool -> ParsecT [Token] Memory IO ([Token])
varDecl global = do
            a <- typeVar
            b <- idToken
            s <- getState
            if global then updateState(insertVarGlobal b (get_default_value a s))
            else updateState(insertVar b (get_default_value a s))
            c <- semiColonToken
            s <- getState
            liftIO (printMem s)
            return ([a] ++ [b] ++ [c])

remainingVarDecls :: Bool -> ParsecT [Token] Memory IO ([Token])
remainingVarDecls global = (varDecls global) <|> (return [])


typeVar :: ParsecT [Token] Memory IO (Token)
typeVar = (numType) <|> (userDefinedType)
          -- <|> (othersTypeToken)

numType :: ParsecT [Token] Memory IO (Token)
numType = 
          (numToken) <|> (numWithSpecificationToken)

userDefinedType :: ParsecT [Token] Memory IO (Token)
userDefinedType = idToken














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
                (f, _) <- subProgramBody
                updateState(popMemStack)
                let allTokens = [a] ++ [b] ++ [c] ++ d ++ [e] ++ f
                updateState(insertSubprogram b d f)
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
                s <- getState
                updateState(insertVar b (get_default_value a s))
                return ([a] ++ [b])

remainingParameters :: ParsecT [Token] Memory IO ([Token])
remainingParameters = (do 
                a <- commaToken
                b <- typeAndId
                return ([a] ++ b)) <|> (return [])


remainingSubPrograms :: ParsecT [Token] Memory IO ([Token])
remainingSubPrograms = (subPrograms) <|> (return [])


subProgramBody :: ParsecT [Token] Memory IO ([Token], Type)
subProgramBody = do 
                    a <- openCurlyBracketsToken
                    b <- varsBlock False
                    (c, returnValue) <- processBlock
                    d <- closeCurlyBracketsToken
                    return ([a] ++ b ++ c ++ [d], returnValue)













-- ------------------------------processBlock---------------------------

processBlock :: ParsecT [Token] Memory IO ([Token], Type)
processBlock = do 
                  a <- processBlockToken
                  b <- colonToken
                  (c, returnValue) <- stmts
                  return ([a] ++ [b] ++ c, returnValue)


stmts :: ParsecT [Token] Memory IO ([Token], Type)
stmts = do
          (a, rv1) <- stmt
          (b, rv2) <- remainingStmts
          let returnValue = if (rv1 == NoValue) then rv2 else rv1
          return (a ++ b, returnValue)


stmt :: ParsecT [Token] Memory IO ([Token], Type)
stmt = singleLineStmt <|> blockStmt

singleLineStmt :: ParsecT [Token] Memory IO ([Token], Type)
singleLineStmt = do
                    (a, returnValue) <- (assign <|> printVar <|> returnStmt)
                    b <- semiColonToken
                    return (a ++ [b], returnValue)

blockStmt :: ParsecT [Token] Memory IO ([Token], Type)
blockStmt = ifMainBlock <|> loopBlock

remainingStmts :: ParsecT [Token] Memory IO ([Token], Type)
remainingStmts = (stmts) <|> (return ([], NoValue))

assign :: ParsecT [Token] Memory IO ([Token], Type)
assign = do
          a <- idAcessor
          b <- equalsToken
          (expT, expVal) <- expression
          ce <- canExecute
          when (ce) (updateState(updateVar a expVal))
          e <- getState
          liftIO (printMem e)
          return (a ++ [b] ++ expT, NoValue)

idAcessor :: ParsecT [Token] Memory IO ([Token])
idAcessor = try idWithField <|> (do
                a <- idToken
                return [a])

idWithField :: ParsecT [Token] Memory IO ([Token])
idWithField = do
                a <- idToken
                b <- dotToken --TODO: create
                c <- idToken
                return ([a] ++ [b] ++ [c])

expression :: ParsecT [Token] Memory IO ([Token], Type)
expression = try binOp <|> unaryExpression

unaryExpression :: ParsecT [Token] Memory IO ([Token], Type)
unaryExpression = do
                    a <- op
                    (tok, val) <- intLit
                    let valWithSign = evalSign a val
                    return ([a] ++ tok, valWithSign)




printVar :: ParsecT [Token] Memory IO ([Token], Type)
printVar = do 
                a <- printToken
                b <- openParToken
                string <- stringLitToken
                comma <- commaToken
                (expT, expVal) <- expression
                g <- closeParToken
                liftIO (printVars string expVal)
                return ([a] ++ [b] ++ [string] ++  [comma] ++ expT ++ [g], NoValue)



ifMainBlock :: ParsecT [Token] Memory IO ([Token], Type)
ifMainBlock = do
                (a, (Boolean valor)) <- ifEvalue
                s <- getState
                when(isRunning s && not valor) (updateState(endBlockExecution))
                (b, rv1) <- block
                when(isRunning s) (
                  if (not valor) then (updateState(beginBlockExecution))
                  else (updateState(endBlockExecution)))
                (c, rv2) <- remainingIfblock valor
                updateState(beginBlockExecution)
                let returnValue = if valor then rv1 else rv2
                return (a ++ b ++ c, returnValue)

ifEvalue :: ParsecT [Token] Memory IO ([Token], Type)
ifEvalue = (do
            a <- ifToken
            b <- openParToken
            (c, valor) <- expression
            d <- closeParToken
            return ([a] ++ [b] ++ c ++ [d], valor))

remainingIfblock :: Bool -> ParsecT [Token] Memory IO ([Token], Type)
remainingIfblock prevValue = try (
                          do 
                          a <- elseToken
                          (b, (Boolean valor)) <- ifEvalue
                          s <- getState
                          when(isRunning s && not valor) (updateState(endBlockExecution))
                          (c, rv1) <- block
                          when(isRunning s) (
                            if(not valor && not prevValue) then (updateState(beginBlockExecution))
                            else (updateState(endBlockExecution)))
                          (d, rv2)<- remainingIfblock valor
                          let returnValue = if valor then rv1 else rv2
                          return ([a] ++ b ++ c ++ d, returnValue)
                        )
                     <|>
                        (
                          do
                          a <- elseToken
                          (b, returnValue) <- block
                          return ([a] ++ b, returnValue)
                        )
                     <|> return ([], NoValue)


-- TODO: make it return possible values from statements
block :: ParsecT [Token] Memory IO ([Token], Type)
block = do
            a <- openCurlyBracketsToken
            (b, returnValue) <- stmts
            c <- closeCurlyBracketsToken
            return ([a] ++ b ++ [c], returnValue)


-- TODO: make it return possible values from statements
loopBlock :: ParsecT [Token] Memory IO ([Token], Type)
loopBlock = do
              a <- loopInitializer

              updateState(endBlockExecution)
              (b, loopCondition, currInput) <- loopConditionAndAction
              updateState(beginBlockExecution)

              when(not loopCondition) (updateState(endBlockExecution))
              (c, returnValue) <- loopUnit currInput
              updateState(beginBlockExecution)

              return (a ++ b ++ c, NoValue)

loopInitializer :: ParsecT [Token] Memory IO ([Token])
loopInitializer = do
                      a <- loopToken
                      b <- openParToken
                      (c, _) <- assign
                      d <- semiColonToken
                      return ([a] ++ [b] ++ c ++ [d])

loopConditionAndAction :: ParsecT [Token] Memory IO ([Token], Bool, [Token])
loopConditionAndAction = do
                      unitLoopStart <- getInput
                      _ <- binOp
                      _ <- semiColonToken

                      (c, _) <- singleLineStmt

                      currInput <- getInput
                      setInput unitLoopStart
                      (a, Boolean loopCondition) <- binOp
                      b <- semiColonToken
                      setInput currInput

                      d <- closeParToken

                      return (a ++ [b] ++ c ++ [d], loopCondition, unitLoopStart)

-- TODO: make it return possible values from statements
loopUnit :: [Token] -> ParsecT [Token] Memory IO ([Token], Type)
loopUnit start = do
                (a, returnValue) <- block

                s <- getState
                currInput <- getInput
                when(isBlockRunning s) (do
                  setInput start
                  (_, loopCondition, _) <- loopConditionAndAction
                  if(loopCondition) then (do
                    _ <- loopUnit start
                    return ())
                  else (setInput currInput)
                  return ())
                return (a, NoValue)


loopExpression :: ParsecT [Token] Memory IO ([Token], Bool)
loopExpression = do
                (a, Boolean loopCondition) <- binOp
                b <- semiColonToken
                (c, _) <- assign
                d <- semiColonToken
                e <- closeParToken
                return (a ++ [b] ++ c ++ [d] ++ [e], loopCondition)

returnStmt :: ParsecT [Token] Memory IO ([Token], Type)
returnStmt = do
                a <- returnToken
                (b, expVal) <- expression
                return ([a] ++ b, expVal)


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
                      eval <- evalOp numb a operandV
                      (tok, val) <- evalueRemaining (eval)
                      return ([a] ++ operandT ++ tok, val)
                    <|> return ([], numb)

operand :: ParsecT [Token] Memory IO ([Token], Type)
operand = try subProgramCall <|>
          (varId <|> intLit)

intLit :: ParsecT [Token] Memory IO ([Token], Type)
intLit = do
            (Int v p) <- intToken
            return ([(Int v p)], Numeric v)

op :: ParsecT [Token] Memory IO (Token)
op = powToken <|> multToken <|> divToken <|> addToken <|> subToken <|> lessToken <|> greaterToken <|> moduleToken <|> equivalentToken <|> differentToken

varId :: ParsecT [Token] Memory IO ([Token], Type)
varId = do 
            a <- idAcessor
            s <- getState
            return (a, evalVar a s)


subProgramCall :: ParsecT [Token] Memory IO ([Token], Type)
subProgramCall = do 
                  (Id funName p) <- idToken
                  b <- openParToken
                  (c, valList) <- argumentList
                  d <- closeParToken
                  ce <- canExecute
                  v <- if ce then (
                    do
                      updateState(pushMemStack)

                      updateState(addParametersToMemory (Id funName p) valList)

                      s <- getState
                      let (_, _, funBody) = findFun funName (getFuns s)
                      inp <- getInput
                      setInput funBody
                      (_, v) <- subProgramBody
                      setInput inp

                      updateState(popMemStack)
                      return (v))
                    else (
                      do
                        let v = NoValue
                        return (v))

                  return ([(Id funName p)] ++ [b] ++ c ++ [d], v)


argumentList :: ParsecT [Token] Memory IO ([Token], [Type])
argumentList = do  
                  (a, valA) <- expression
                  (b, valList) <- remainingArguments
                  return (a ++ b, valA:valList)

remainingArguments :: ParsecT [Token] Memory IO ([Token], [Type])
remainingArguments = (do  
                  a <- commaToken
                  (b, valList) <- argumentList
                  return ([a] ++ b, valList)) <|> (return ([], []))




----------processBlock - auxiliary functions----------

-- TODO: finalizar implementação
evalSign :: Token -> Type -> Type
evalSign _ t = t


canExecute :: ParsecT [Token] Memory IO (Bool)
canExecute = do
                s <- getState
                return (isRunning s && isBlockRunning s)


-- TODO: find way to throw exception in type mismatch
evalOp :: Type -> Token -> Type -> ParsecT [Token] Memory IO (Type)
evalOp (Numeric x) (Pow _) (Numeric y) = return (Numeric (x ^ y))
evalOp (Numeric x) (Mult _) (Numeric y) = return (Numeric (x * y))
evalOp (Numeric x) (Div _) (Numeric y) = return (Numeric (x `div` y))
evalOp (Numeric x) (Add _) (Numeric y) = return (Numeric (x + y))
evalOp (Numeric x) (Sub _) (Numeric y) = return (Numeric (x - y))
evalOp (Numeric x) (Greater _) (Numeric y) = return (Boolean (x > y))
evalOp (Numeric x) (Less _) (Numeric y) = return (Boolean (x < y))
evalOp (Numeric x) (Mod _) (Numeric y) = return (Numeric (x `mod` y))
evalOp (Numeric x) (Equiv _) (Numeric y) = return (Boolean (x == y))
evalOp (Numeric x) (Diff _) (Numeric y) = return (Boolean (x /= y))
evalOp (UserDefined (name1, fields1, funs1)) op (UserDefined (name2, fields2, funs2)) =
  if name1 == name2 then evalUserTypeOp (UserDefined (name1, fields1, funs1)) (UserDefined (name2, fields2, funs2)) (opToSymbol op)
  else error(typeMismatchException name1 (0, 0))

evalUserTypeOp :: Type -> Type -> String -> ParsecT [Token] Memory IO (Type)
evalUserTypeOp (UserDefined (name1, fields1, (opName, param, body):funs1)) operand2 op =
  if opName == op then callOperation (UserDefined (name1, fields1, (opName, param, body):funs1)) operand2 param body
  else evalUserTypeOp (UserDefined (name1, fields1, funs1)) operand2 op

callOperation :: Type -> Type -> [Token] -> [Token] -> ParsecT [Token] Memory IO (Type)
callOperation a1 a2 params body =
  do
     ce <- canExecute
     v <- if ce then (
       do
         updateState(pushMemStack)

         updateState(addParametersToMemoryAux [a1, a2] params)

         inp <- getInput
         setInput body
         (_, v) <- subProgramBody
         setInput inp

         updateState(popMemStack)
         return (v))
       else (
         do
           let v = NoValue
           return (v))

     return (v)

evalVar :: [Token] -> Memory -> Type
evalVar t mem = evalVarAux t (getCurrentVars mem)

evalVarAux :: [Token] -> Variables -> Type
evalVarAux ((Id x p):ts) [] = error(notFoundException x p)
evalVarAux ((Id x p):[]) ((name, ty):lv) =
  if x == name then ty
  else evalVarAux ((Id x p):[]) lv

evalVarAux [(Id varName p), (Dot p2), varField] ((name, ty):lv) =
  if varName == name then getFieldFromVar varField ty
  else evalVarAux [(Id varName p), (Dot p2), varField] lv


getFieldFromVar :: Token -> Type -> Type
getFieldFromVar fieldName (UserDefined (_, fields, _))  = getFieldFromVarAux fieldName fields
getFieldFromVar (Id fieldName p) _ = error(fieldMismatchException fieldName p)

getFieldFromVarAux :: Token -> Variables -> Type
getFieldFromVarAux (Id fieldName p) [] = error(fieldMismatchException fieldName p)
getFieldFromVarAux (Id fieldName p) ((name, t):fields) =
  if fieldName == name then t
  else getFieldFromVarAux (Id fieldName p) fields










-------------- Memory functions ------------------

-- TODO: Exibir erro caso não encontre função
findFun :: String -> Functions -> Function
findFun name ((n, params, body):lf) = if name == n then (n, params, body)
                                           else findFun name lf


addParametersToMemory :: Token -> [Type] -> Memory -> Memory
addParametersToMemory (Id name _) args ((varsG, funs, uts), s, ir, irb) = addParametersToMemoryAux args params ((varsG, funs, uts), s, ir, irb)
    where (_, params, _) = findFun name funs

addParametersToMemoryAux :: [Type] -> [Token] -> Memory -> Memory
addParametersToMemoryAux (val:args) ((Id paramName pp):params) mem =
    if(paramNameIsUserTypeName) then addParametersToMemoryAux (val:args) params mem
    else addParametersToMemoryAux args params updatedMem
        where
            updatedMem = insertVar (Id paramName pp) val mem
            paramNameIsUserTypeName = doesUserTypeExist paramName (getUserTypes mem)

-- In case of a collon or type in the paramList
addParametersToMemoryAux args ((Comma _):params) mem = addParametersToMemoryAux args params mem
addParametersToMemoryAux args ((Num _ _):params) mem = addParametersToMemoryAux args params mem
addParametersToMemoryAux [] [] mem = mem


doesUserTypeExist :: String -> UserTypes -> Bool
doesUserTypeExist name [] = False
doesUserTypeExist name ((UserDefined (nameUt, _, _)):uts) =
  if name == nameUt then True
  else doesUserTypeExist name uts


get_default_value :: Token -> Memory -> Type
get_default_value (Num "num" _) _ = Numeric 0
get_default_value (Id name p) mem = findUserType (Id name p) mem


insertVarGlobal :: Token -> Type -> Memory -> Memory
insertVarGlobal (Id name _) varType ((vars, funs, uts), s, ir, irb) = ((updatedVars, funs, uts), s, ir, irb)
                                                    where updatedVars = addVarToMemory name varType vars

insertVar :: Token -> Type -> Memory -> Memory
insertVar (Id name _) varType (g, s:ls, ir, irb) = (g, (updatedVars:ls), ir, irb)
                                                    where updatedVars = addVarToMemory name varType s

addVarToMemory :: String -> Type -> Variables -> Variables
addVarToMemory name varType vars = (name, varType):vars

insertSubprogram :: Token -> [Token] -> [Token] -> Memory -> Memory
insertSubprogram (Id name _) parametersList allTokens ((vars, funs, uts), s, ir, irb) = ((vars, updatedFuns, uts), s, ir, irb)
    where updatedFuns = addSubprogramToMemory name parametersList allTokens funs

addSubprogramToMemory :: String -> [Token] -> [Token] -> Functions -> Functions
addSubprogramToMemory name parametersList body funs = (name, parametersList, body):funs

-- Exceptions
notFoundException :: String -> (Int, Int) -> String
notFoundException name (l, c) = "Variable " ++ name  ++ " not found " ++ "at line " ++ show l ++ ", column " ++ show c

nameInUseException :: String -> (Int, Int) -> String
nameInUseException name (l, c) = "Name " ++ name  ++ " already in use at line " ++ show l ++ ", column " ++ show c

fieldMismatchException :: String -> (Int, Int) -> String
fieldMismatchException name (l, c) = "Variable at line" ++ show l ++ ", column " ++ show c ++ " does not have field named " ++ name

typeMismatchException :: String -> (Int, Int) -> String
typeMismatchException name (l, c) = "Type not compatible in assignment of " ++ name ++ " at line" ++ show l ++ ", column " ++ show c



-- Recebe um idToken, um typeToken, a memória e retorna a memória atualizada
updateVar :: [Token] -> Type -> Memory -> Memory
updateVar tok val ((gVars, funs, uts), s:ls, ir, irb) = do
  let newGlobals = updateVarAux tok val gVars
  if(newGlobals /= []) then ((newGlobals, funs, uts), s:ls, ir, irb)
  else (do
    let newStackVars = updateVarAux tok val s
    if(newStackVars /= []) then ((gVars, funs, uts), newStackVars:ls, ir, irb)
    else error(notFoundException name p))
    where ((Id name p):_) = tok
updateVar tok val ((gVars, funs, uts), [], ir, irb) = do
  let global = updateVarAux tok val gVars
  if(global /= []) then ((global, funs, uts), [], ir, irb)
  else error(notFoundException name p)
  where ((Id name p):_) = tok


updateVarAux :: [Token] -> Type -> Variables -> Variables
updateVarAux ((Id id1 p):ts) _ [] = []
updateVarAux ((Id id1 p):[]) newVal ((name, oldVal):lv) =
                               if id1 == name then (name, updatedVal):lv
                               else (if keepLooking /= [] then ((name, oldVal):keepLooking) else [])
                               where
                                 keepLooking = updateVarAux ((Id id1 p):[]) newVal lv
                                 updatedVal = checkType oldVal newVal (Id id1 p)

updateVarAux [(Id varName p), (Dot p2), fieldName] newVal ((name, (UserDefined (utName, fields, funs))):lv) =
  if varName == name then ((name, (UserDefined (utName, updatedFields, funs))):lv)
  else (if keepLooking /= [] then (name, (UserDefined (utName, fields, funs))):keepLooking else [])
  where
   keepLooking = updateVarAux [(Id varName p), (Dot p2), fieldName] newVal lv
   updatedFields = updateVarAux (fieldName:[]) newVal fields

checkType :: Type -> Type -> Token -> Type
checkType oldVal newVal (Id name p) = if oldVal == newVal then newVal else error(typeMismatchException name p)


insertUserType :: Token -> Memory -> Memory
insertUserType idT ((v, f, uts), s, ir, irb) = ((v, f, newUts), s, ir, irb)
  where newUts = insertUserTypeAux idT uts

insertUserTypeAux :: Token -> UserTypes -> UserTypes
insertUserTypeAux (Id name p) [] = (UserDefined (name, [], [])):[]
insertUserTypeAux (Id name p) ((UserDefined (utName, vars, funs)):uts) =
  if name == utName then error(nameInUseException name p)
  else ((UserDefined (utName, vars, funs)):(insertUserTypeAux (Id name p) uts))


findUserType :: Token -> Memory -> Type
findUserType idT mem = findUserTypeAux idT uts
  where uts = getUserTypes mem

findUserTypeAux :: Token -> UserTypes -> Type
findUserTypeAux (Id nameSrc p) [] = error(notFoundException nameSrc p)
findUserTypeAux (Id nameSrc p) ((UserDefined (nameTgt, v, f)):uts) =
    if nameSrc == nameTgt then (UserDefined (nameTgt, v, f))
    else findUserTypeAux (Id nameSrc p) uts

updateUserTypeVars :: Token -> Variables -> Memory -> Memory
updateUserTypeVars name vars ((v, f, uts), s, ir, irb) = ((v, f, updatedUts) , s, ir, irb)
  where updatedUts = updateUserTypeVarsAux name vars uts

updateUserTypeVarsAux :: Token -> Variables -> UserTypes -> UserTypes
updateUserTypeVarsAux (Id nameSrc p) vars ((UserDefined (nameTgt, v, f)):uts) =
  if nameSrc == nameTgt then (UserDefined (nameTgt, vars, f)):uts
  else (UserDefined (nameTgt, v, f)):(updateUserTypeVarsAux (Id nameSrc p) vars uts)

updateUserTypeFuns :: Token -> Functions -> Memory -> Memory
updateUserTypeFuns name funs ((v, f, uts), s, ir, irb) = ((v, f, updatedUts) , s, ir, irb)
  where updatedUts = updateUserTypeFunsAux name funs uts

updateUserTypeFunsAux :: Token -> Functions -> UserTypes -> UserTypes
updateUserTypeFunsAux (Id nameSrc p) funs ((UserDefined (nameTgt, v, f)):uts) =
  if nameSrc == nameTgt then (UserDefined (nameTgt, v, funs)):uts
  else (UserDefined (nameTgt, v, f)):(updateUserTypeFunsAux (Id nameSrc p) funs uts)


-- invocação do parser para o símbolo de partida 
parser :: [Token] -> IO (Either ParseError [Token])
parser tokens = runParserT program (([], [], []), [], False, False) "Error message" tokens

main :: IO ()
main = do a <- getArgs
          case unsafePerformIO (parser (getTokens (a !! 0))) of
            { Left err -> print err; 
              Right ans -> print ans
            }
