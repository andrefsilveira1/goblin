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
  $digit+                              { \s -> Int (read s) }
  $alpha [$alpha $digit \_ \']*        { \s -> Id s }
  \" $alpha [$alpha $digit ! \_ \']* \"  { \s -> String s}

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
  Program |
  Var     |
  Begin   |
  End     |
  Colon   |
  SemiColon |
  Assign    | 
  If  |
  Then |
  Write |
  Greater |
  Type String |
  Id String |
  Int Int |
  String String
  deriving (Eq,Show)

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}