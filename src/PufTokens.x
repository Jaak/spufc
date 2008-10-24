{
module PufTokens (Token(..), alexScanTokens, main) where
  
import AST (Builtin(..))
}

%wrapper "basic"

tokens :-
  -- ignore whitespace
  $white                  ;
  
  -- if then else
  if                      { const If }
  then                    { const Then }
  else                    { const Else }
  
  -- function
  fn                      { const Fn }
  \,                      { const Comma }
  =>                      { const MapsTo}
  
  -- let's
  let                     { const Let }
  letrec                  { const Letrec }
  =                       { const Eq }
  \;                      { const Semicolon }
  
  -- builtin operators
  ==                      { const $ Bi BEq }
  \/=                     { const $ Bi BNe }
  \<                      { const $ Bi BLt }
  \>                      { const $ Bi BGt }
  >=                      { const $ Bi BGe }
  \<=                     { const $ Bi BLe }
  
  -- literals and identificators
  [\-]?[0-9]+             { Lit . read }
  [a-zA-Z_][a-zA-Z0-9'_]*  { Id }
  
  -- error case
  .                       { Error }
{

data Token = Lit Integer
           | Id  String 
           | Bi  Builtin
           | Fn
           | If
           | Then
           | Else
           | MapsTo
           | Let 
           | Letrec
           | In
           | Eq
           | Comma
           | Semicolon
           | Error String
           deriving (Show,Eq)

main = do
  s <- getContents
  print (alexScanTokens s)
  
}