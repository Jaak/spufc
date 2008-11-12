{
module PufTokens (RawToken(..), Token(..), Pos, alexScanTokens, Builtin(..)) where

import AST (Builtin(..))

}

%wrapper "posn-bytestring"

$alpha0 = [a-z A-Z 0-9 \xAA \xBA \xC0-\xD6 \xD8-\xF6 \xF8-\xFF]

$alpha1 = [\x00-\xF5 \xFA-\xFF]

@unialpha = ($alpha0 | \x01 $alpha1)

@comment1  =  \/ \* ( \/ | (. | \n) | \* (. | \n) )*  \*\/
@comment2  = \/\/ .* \n
@comment   = @comment1 | @comment2

tokens :-
  -- ignore whitespace
  $white                  ;
  @comment                ;
  
  -- includes
  \#include $white* (@unialpha | \.)* { \ p x -> Include (drop 9 $ ByteString.unpack x) (tr p) }
  
  -- if then else
  if                      { \ p x -> JustToken If   (tr p) }
  then                    { \ p x -> JustToken Then (tr p) }
  else                    { \ p x -> JustToken Else (tr p) }
  
  -- parens/brackets
  \(                      { \ p x -> JustToken ParenLeft    (tr p) }
  \)                      { \ p x -> JustToken ParenRight   (tr p) }
  \[                      { \ p x -> JustToken BracketLeft  (tr p) }
  \]                      { \ p x -> JustToken BracketRight (tr p) }
  
  -- function
  fn                      { \ p x -> JustToken Fn     (tr p) }
  \,                      { \ p x -> JustToken Comma  (tr p) }
  \->                     { \ p x -> JustToken MapsTo (tr p) }
  
  -- builtin operators
  neg                     { \ p x -> JustToken (Bi UNeg) (tr p) }
  not                     { \ p x -> JustToken (Bi UNot) (tr p) }
  \#                      { \ p x -> JustToken Sel       (tr p) }
  \-                      { \ p x -> JustToken (Bi BSub) (tr p) }
  \+                      { \ p x -> JustToken (Bi BAdd) (tr p) }
  \*                      { \ p x -> JustToken (Bi BMul) (tr p) }
  \/                      { \ p x -> JustToken (Bi BDiv) (tr p) }
  \%                      { \ p x -> JustToken (Bi BMod) (tr p) }
  ==                      { \ p x -> JustToken (Bi BEq ) (tr p) }
  \/=                     { \ p x -> JustToken (Bi BNe ) (tr p) }
  \<                      { \ p x -> JustToken (Bi BLt ) (tr p) }
  \>                      { \ p x -> JustToken (Bi BGt ) (tr p) }
  >=                      { \ p x -> JustToken (Bi BGe ) (tr p) }
  \<=                     { \ p x -> JustToken (Bi BLe ) (tr p) }
  
  -- let's
  let                     { \ p x -> JustToken Let       (tr p) }
  letrec                  { \ p x -> JustToken Letrec    (tr p) }
  =                       { \ p x -> JustToken Eq        (tr p) }
  \;                      { \ p x -> JustToken Semicolon (tr p) }
  in                      { \ p x -> JustToken In        (tr p) }
  
  -- literals and identificators
  [0-9]+                  { \ p x -> JustToken (Lit $ read $ ByteString.unpack x)  (tr p) }
  @unialpha @unialpha*    { \ p x -> JustToken (Id  $ ByteString.unpack x)         (tr p) }
  
  -- error case
  .                       { \ p x -> Error (ByteString.unpack x) (tr p) }
{

type Offs = Int
type Line = Int
type Col  = Int
type Pos = (Offs, Line, Col)

tr (AlexPn offs line col) = (offs, line, col)

data Token = Lit Int
           | Id  String
           | Bi  Builtin
           | Sel
           | Fn
           | If | Then | Else
           | MapsTo
           | Let 
           | Letrec
           | In
           | Eq
           | Comma       | Semicolon
           | ParenLeft   | ParenRight
           | BracketLeft | BracketRight
           deriving (Show, Eq)

data RawToken = JustToken  Token  Pos
              | Include    String Pos
              | Error      String Pos
              deriving (Show, Eq)
  
}
