{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "basic"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-

  $white+                              ;
  "--".*                               ;
  program                              { \s -> Program }
  var                                  { \s -> Var }
  begin                                { \s -> Begin}
  end                                  { \s -> End}
  :                                    { \s -> Colon}
  ";"                                  { \s -> SemiColon}
  int                                  { \s -> Type s}
  :=                                   { \s -> Assign}
  if                                   { \s -> If}
  then                                 { \s -> Then}
  write                                { \s -> Write}
  >                                    { \s -> Greater}
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }
  $digit+                              { \s -> Int (read s) }
  \" $alpha [$alpha $digit ! \_ \']* \"{ \s -> String s}
  function                             { \s -> Function } 
  "("                                  { \s -> OpenPar }  
  ")"                                  { \s -> ClosePar}
  Float                                { \s -> Float s}
  Boolean                              { \s -> Boolean s}
  Char                                 { \s -> Char s}  

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Program         |
  Var             |
  Begin           |
  End             |
  Colon           |
  SemiColon       |
  Type     String |
  Assign          | 
  If              |
  Then            |
  Write           |
  Greater         |
  Id       String |
  Int      Int    |
  String   String |
  Function        |
  OpenPar         |
  ClosePar        |
  Float    String |
  Boolean  String |
  Char     String 
  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}