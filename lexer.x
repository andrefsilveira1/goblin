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
  :                                    { \s -> Colon}
  ";"                                  { \s -> SemiColon}
  int                                  { \s -> Type s}
  if                                   { \s -> If}
  then                                 { \s -> Then}
  write                                { \s -> Write}
  >                                    { \s -> Greater}
  $digit+                              { \s -> Int (read s) }
  \" $alpha [$alpha $digit ! \_ \']* \"{ \s -> String s}
  "("                                  { \s -> OpenPar }  
  ")"                                  { \s -> ClosePar}
  Float                                { \s -> Float s}
  Boolean                              { \s -> Boolean s}
  Char                                 { \s -> Char s}  
  vars                                 { \s -> VarsBlock}
  subprograms                          { \s -> SubprogramsBlock}
  process                              { \s -> ProcessBlock}
  "{"                                  { \s -> OpenCurlyBrackets}
  "}"                                  { \s -> CloseCurlyBrackets}
  =                                    { \s -> Equals}
  num                                  { \s -> Num s}
  tempTemp                             { \s -> NumWithSpecification}
  ","                                  { \s -> Comma}
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }
  "+"                                  { \s -> Add}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Program              |
  Var                  |
  Begin                |
  End                  |
  Colon                |
  SemiColon            |
  Type     String      |
  Assign               | 
  If                   |
  Then                 |
  Write                |
  Greater              |
  Id       String      |
  Int      Int         |
  String   String      |
  Function             |
  OpenPar              |
  ClosePar             |
  Float    String      |
  Char     String      |
  Boolean  String      |
  VarsBlock            |
  SubprogramsBlock     |
  ProcessBlock         |
  OpenCurlyBrackets    |
  CloseCurlyBrackets   |
  Equals               |
  Num      String      |
  NumWithSpecification |
  Comma                |
  Add
  deriving (Eq,Show)   

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}