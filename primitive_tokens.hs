module PrimitiveTokens where

import Lexer
import Text.Parsec

-- parsers para os tokens


idToken :: ParsecT [Token] st IO (Token)
idToken = tokenPrim show update_pos get_token where
  get_token (Id x p) = Just (Id x p)
  get_token _      = Nothing

semiColonToken :: ParsecT [Token] st IO (Token)
semiColonToken = tokenPrim show update_pos get_token where
  get_token (SemiColon p) = Just (SemiColon p)
  get_token _         = Nothing

colonToken :: ParsecT [Token] st IO (Token)
colonToken = tokenPrim show update_pos get_token where
  get_token (Colon p) = Just (Colon p)
  get_token _     = Nothing


intToken :: ParsecT [Token] st IO (Token)
intToken = tokenPrim show update_pos get_token where
  get_token (Int x p) = Just (Int x p)
  get_token _       = Nothing

-- typeToken = tokenPrim show update_pos get_token where
--   get_token (Type (x) p) = Just (Type (x) p)
--   get_token _        = Nothing

openParToken :: ParsecT [Token] st IO (Token)
openParToken = tokenPrim show update_pos get_token where
  get_token (OpenPar p) = Just (OpenPar p)
  get_token _     = Nothing

closeParToken :: ParsecT [Token] st IO (Token)
closeParToken = tokenPrim show update_pos get_token where
  get_token (ClosePar p) = Just (ClosePar p)
  get_token _   = Nothing

varsBlockToken :: ParsecT [Token] st IO (Token)
varsBlockToken = tokenPrim show update_pos get_token where
  get_token (VarsBlock p) = Just (VarsBlock p)
  get_token _   = Nothing

processBlockToken :: ParsecT [Token] st IO (Token)
processBlockToken = tokenPrim show update_pos get_token where
  get_token (ProcessBlock p) = Just (ProcessBlock p)
  get_token _   = Nothing

subprogramsBlockToken :: ParsecT [Token] st IO (Token)
subprogramsBlockToken = tokenPrim show update_pos get_token where
  get_token (SubprogramsBlock p) = Just (SubprogramsBlock p)
  get_token _   = Nothing

openCurlyBracketsToken :: ParsecT [Token] st IO (Token)
openCurlyBracketsToken = tokenPrim show update_pos get_token where
  get_token (OpenCurlyBrackets p) = Just (OpenCurlyBrackets p)
  get_token _   = Nothing

closeCurlyBracketsToken :: ParsecT [Token] st IO (Token)
closeCurlyBracketsToken = tokenPrim show update_pos get_token where
  get_token (CloseCurlyBrackets p) = Just (CloseCurlyBrackets p)
  get_token _   = Nothing

equalsToken :: ParsecT [Token] st IO (Token)
equalsToken = tokenPrim show update_pos get_token where
  get_token (Equals p) = Just (Equals p)
  get_token _   = Nothing

numToken :: ParsecT [Token] st IO (Token)
numToken = tokenPrim show update_pos get_token where
  get_token (Num x p) = Just (Num x p)
  get_token _   = Nothing

numWithSpecificationToken :: ParsecT [Token] st IO (Token)
numWithSpecificationToken = tokenPrim show update_pos get_token where
  get_token (NumWithSpecification p) = Just (NumWithSpecification p)
  get_token _   = Nothing

commaToken :: ParsecT [Token] st IO (Token)
commaToken = tokenPrim show update_pos get_token where
  get_token (Comma p) = Just (Comma p)
  get_token _   = Nothing

addToken :: ParsecT [Token] st IO (Token)
addToken = tokenPrim show update_pos get_token where
  get_token (Add p) = Just (Add p)
  get_token _   = Nothing

subToken :: ParsecT [Token] st IO (Token)
subToken = tokenPrim show update_pos get_token where
  get_token (Sub p) = Just (Sub p)
  get_token _   = Nothing

multToken :: ParsecT [Token] st IO (Token)
multToken = tokenPrim show update_pos get_token where
  get_token (Mult p) = Just (Mult p)
  get_token _   = Nothing

powToken :: ParsecT [Token] st IO (Token)
powToken = tokenPrim show update_pos get_token where
  get_token (Pow p) = Just (Pow p)
  get_token _   = Nothing

divToken :: ParsecT [Token] st IO (Token)
divToken = tokenPrim show update_pos get_token where
  get_token (Div p) = Just (Div p)
  get_token _   = Nothing

moduleToken :: ParsecT [Token] st IO (Token)
moduleToken = tokenPrim show update_pos get_token where
  get_token (Mod p) = Just (Mod p)
  get_token _ = Nothing

lessToken :: ParsecT [Token] st IO (Token)
lessToken = tokenPrim show update_pos get_token where
  get_token (Less p) = Just (Less p)
  get_token _    = Nothing

greaterToken :: ParsecT [Token] st IO (Token)
greaterToken = tokenPrim show update_pos get_token where
  get_token (Greater p) = Just (Greater p)
  get_token _       = Nothing

equivalentToken :: ParsecT [Token] st IO (Token)
equivalentToken = tokenPrim show update_pos get_token where
  get_token (Equiv p) = Just (Equiv p)
  get_token _       = Nothing

differentToken :: ParsecT [Token] st IO (Token)
differentToken = tokenPrim show update_pos get_token where
  get_token (Diff p) = Just (Diff p)
  get_token _       = Nothing

printToken :: ParsecT [Token] st IO (Token)
printToken = tokenPrim show update_pos get_token where
  get_token (Print p) = Just (Print p)
  get_token _    = Nothing

returnToken :: ParsecT [Token] st IO (Token)
returnToken = tokenPrim show update_pos get_token where
  get_token (Return p) = Just (Return p)
  get_token _   = Nothing

ifToken :: ParsecT [Token] st IO (Token)
ifToken = tokenPrim show update_pos get_token where
  get_token (If p) = Just (If p)
  get_token _    = Nothing

elseToken :: ParsecT [Token] st IO (Token)
elseToken = tokenPrim show update_pos get_token where
  get_token (Else p) = Just (Else p)
  get_token _ = Nothing

loopToken :: ParsecT [Token] st IO (Token)
loopToken = tokenPrim show update_pos get_token where
  get_token (Loop p) = Just (Loop p)
  get_token _ = Nothing

typesBlockToken :: ParsecT [Token] st IO (Token)
typesBlockToken = tokenPrim show update_pos get_token where
  get_token (TypesBlock p) = Just (TypesBlock p)
  get_token _ = Nothing

fieldsBlockToken :: ParsecT [Token] st IO (Token)
fieldsBlockToken = tokenPrim show update_pos get_token where
  get_token (FieldsBlock p) = Just (FieldsBlock p)
  get_token _ = Nothing

operationsBlockToken :: ParsecT [Token] st IO (Token)
operationsBlockToken = tokenPrim show update_pos get_token where
  get_token (OperationsBlock p) = Just (OperationsBlock p)
  get_token _ = Nothing






floatToken :: ParsecT [Token] st IO (Token)
floatToken = tokenPrim show update_pos get_token where
  get_token (Float x p)   = Just (Float x p)
  get_token _           = Nothing


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
