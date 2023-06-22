{
module Lexer where

import System.IO
import System.IO.Unsafe
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z] -- alphabetic characters

tokens :-

  $white+                               ;
  "//".*                                ;
  :                                     { \p s -> Colon (getLC p)}
  ";"                                   { \p s -> SemiColon (getLC p)}
  int                                   { \p s -> Type s (getLC p)}
  if                                    { \p s -> If (getLC p)}
  then                                  { \p s -> Then (getLC p)}
  write                                 { \p s -> Write (getLC p)}
  >                                     { \p s -> Greater (getLC p)}
  $digit+                               { \p s -> Int (read s)  (getLC p)}
  \" $alpha [$alpha $digit ! \_ \']* \" { \p s -> String s (getLC p)}
  "("                                   { \p s -> OpenPar  (getLC p)}  
  ")"                                   { \p s -> ClosePar (getLC p)}
  Float                                 { \p s -> Float s (getLC p)}
  Boolean                               { \p s -> Boolean s (getLC p)}
  Char                                  { \p s -> Char s (getLC p)}  
  vars                                  { \p s -> VarsBlock (getLC p)}
  subprograms                           { \p s -> SubprogramsBlock (getLC p)}
  process                               { \p s -> ProcessBlock (getLC p)}
  "{"                                   { \p s -> OpenCurlyBrackets (getLC p)}
  "}"                                   { \p s -> CloseCurlyBrackets (getLC p)}
  =                                     { \p s -> Equals (getLC p)}
  num                                   { \p s -> Num s (getLC p)}
  tempTemp                              { \p s -> NumWithSpecification (getLC p)}
  ","                                   { \p s -> Comma (getLC p)}
  $alpha [$alpha $digit \_ \']*         { \p s -> Id s  (getLC p)}
  "+"                                   { \p s -> Add (getLC p)}
  "*"                                   { \p s -> Mult (getLC p)}
  "-"                                   { \p s -> Sub (getLC p)}
  "^"                                   { \p s -> Pow (getLC p)}

{
-- Each action has type :: AlexPosn -> String -> Token

-- The token type:
data Token =
  Colon                (Int, Int) |
  SemiColon            (Int, Int) |
  Type     String      (Int, Int) |
  If                   (Int, Int) |
  Then                 (Int, Int) |
  Write                (Int, Int) |
  Greater              (Int, Int) |
  Id       String      (Int, Int) |
  Int      Int         (Int, Int) |
  String   String      (Int, Int) |
  OpenPar              (Int, Int) |
  ClosePar             (Int, Int) |
  Float    String      (Int, Int) |
  Char     String      (Int, Int) |
  Boolean  String      (Int, Int) |
  VarsBlock            (Int, Int) |
  SubprogramsBlock     (Int, Int) |
  ProcessBlock         (Int, Int) |
  OpenCurlyBrackets    (Int, Int) |
  CloseCurlyBrackets   (Int, Int) |
  Equals               (Int, Int) |
  Num      String      (Int, Int) |
  NumWithSpecification (Int, Int) |
  Comma                (Int, Int) |
  Add                  (Int, Int) |
  Sub                  (Int, Int) |
  Mult                 (Int, Int) |
  Pow                  (Int, Int) 
  deriving (Eq,Show)   

getLC (AlexPn _ l c) = (l, c) 

getTokens fn = unsafePerformIO (getTokensAux fn)

getTokensAux fn = do {fh <- openFile fn ReadMode;
                      s <- hGetContents fh;
                      return (alexScanTokens s)}
}