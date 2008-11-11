{
module PufTokens (Token(..), alexScanTokens, main, Builtin(..)) where

import AST (Builtin(..))

}

%wrapper "basic-bytestring"

$alpha0 = [a-z A-Z 0-9 \xAA \xBA \xC0-\xD6 \xD8-\xF6 \xF8-\xFF]

$alpha1 = [\x00-\xF5 \xFA-\xFF]

@unialpha = ($alpha0 | \x01 $alpha1)

tokens :-
  -- ignore whitespace
  $white                  ;
  
  -- if then else
  if                      { const If }
  then                    { const Then }
  else                    { const Else }
  
  -- parens/brackets
  \(                      { const ParenLeft}
  \)                      { const ParenRight}
  \[                      { const BracketLeft}
  \]                      { const BracketRight}
  
  -- function
  fn                      { const Fn     }
  \,                      { const Comma  }
  \->                     { const MapsTo }
  
  -- builtin operators
  neg                     { const $ Bi UNeg}
  not                     { const $ Bi UNot}
  \#                      { const $ Sel }
  \-                      { const $ Bi BSub}
  \+                      { const $ Bi BAdd}
  \*                      { const $ Bi BMul}
  \/                      { const $ Bi BDiv}
  \%                      { const $ Bi BMod}
  ==                      { const $ Bi BEq }
  \/=                     { const $ Bi BNe }
  \<                      { const $ Bi BLt }
  \>                      { const $ Bi BGt }
  >=                      { const $ Bi BGe }
  \<=                     { const $ Bi BLe }
  
  -- let's
  let                     { const Let       }
  letrec                  { const Letrec    }
  =                       { const Eq        }
  \;                      { const Semicolon }
  
  -- literals and identificators
  [0-9]+                  { Lit . read . ByteString.unpack }
  @unialpha @unialpha*    { Id . ByteString.unpack         }
  
  -- error case
  .                       { Error . ByteString.unpack }
{

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
           | LeftParen
           | RightParen
           | Comma
           | Semicolon
           | ParenLeft | ParenRight
           | BracketLeft | BracketRight
           | Error String
           deriving (Show,Eq)

main = do
  s <- ByteString.getContents
  print $ (alexScanTokens s)
  
}
