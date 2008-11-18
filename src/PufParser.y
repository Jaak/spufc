{
module PufParser (parseFile, parseExpr) where

import PufTokens
import qualified AST
import qualified Data.ByteString.Lazy.Char8 as ByteString

}

%name expr expr
%name parse parse
%tokentype {(Token,Pos,FilePath)}

%token
  lit      { (Lit $$       ,_,_)}
  id       { (Id  $$       ,_,_)}
  '#'      { (Sel          ,_,_)}
  '!'      { (Bi UNot      ,_,_)}
  neg      { (Bi UNeg      ,_,_)}
  '+'      { (Bi BAdd      ,_,_)}
  '-'      { (Bi BSub      ,_,_)}
  '*'      { (Bi BMul      ,_,_)}
  '/'      { (Bi BDiv      ,_,_)}
  '%'      { (Bi BMod      ,_,_)}
  '<'      { (Bi BLt       ,_,_)}
  '<='     { (Bi BLe       ,_,_)}
  '=='     { (Bi BEq       ,_,_)}
  '/='     { (Bi BNe       ,_,_)}
  '>'      { (Bi BGt       ,_,_)}
  '>='     { (Bi BGe       ,_,_)}
  '||'     { (Bi BOr       ,_,_)}
  '&&'     { (Bi BAnd      ,_,_)}
  fn       { (Fn           ,_,_)}
  if       { (If           ,_,_)}
  then     { (Then         ,_,_)}
  else     { (Else         ,_,_)}
  '->'     { (MapsTo       ,_,_)}
  let      { (Let          ,_,_)}
  letrec   { (Letrec       ,_,_)}
  in       { (In           ,_,_)}
  case     { (Case         ,_,_)}
  of       { (Of           ,_,_)}
  '='      { (Eq           ,_,_)}
  ','      { (Comma        ,_,_)}
  ';'      { (Semicolon    ,_,_)}
  ':'      { (Colon        ,_,_)}
  '('      { (ParenLeft    ,_,_)}
  ')'      { (ParenRight   ,_,_)}
  '['      { (BracketLeft  ,_,_)}
  ']'      { (BracketRight ,_,_)}
%%

parse :: { [AST.Binding Name] }
  : program  { $1 } 

program :: { [AST.Binding Name] }
  :                   {         [] }
  | program rdecl     { $1 ++ [$2] }
  
rdecl :: { (AST.Binding Name) }
  : id flhs '=' expr ';' { ($1, case $2 of [] -> $4 ; xs -> AST.Abs xs $4) }

rdecl_s :: { [AST.Binding Name] }
  :               {         [] }
  | rdecl_s rdecl { $1 ++ [$2] }
  
ldecl :: { (AST.Decl Name) }
  : id flhs '=' expr ';' { AST.Single $1 $ case $2 of [] -> $4 ; xs -> AST.Abs xs $4 }
  | tlhs    '=' expr ';' { AST.Tuple  $1 $3 }

ldecl_s :: { [AST.Decl Name] }
  :               {         [] }
  | ldecl_s ldecl { $1 ++ [$2] }

flhs :: { [Name] }
  :           {         [] }
  | flhs id   { $1 ++ [$2] }

id_list :: { [Name] }
  : id              {       [$1] }
  | id_list ',' id  { $1 ++ [$3] }

id_s :: { [Name] }
  :             {         [] }
  | id_s id  { $1 ++ [$2] }
  
tlhs :: { [Name] }
  : '(' id_list ')' { $2 }

expr_list :: { [AST.AST Name] }
  : expr                {       [$1] }
  | expr_list ',' expr  { $1 ++ [$3] }

expr_pr :: { AST.AST Name}
  :  lit                                { AST.Lit $1 }
  |  id                                 { AST.Var $1 }
  | '(' ')'                             { AST.MkTuple []}   
  | '(' expr ')'                        { $2 }
  | '(' expr_list ',' expr ',' expr ')' { AST.MkTuple ($2 ++ [$4,$6])}   
  | '[' ']'                             { AST.Nil  }  
  | '[' expr_list ']'                   { foldr AST.Cons AST.Nil $2 }  

expr_uop :: { AST.AST Name }
  : uop expr_pr               { AST.Builtin $1 [$2] }
  | '#' lit expr              { AST.Select $2 $3 }
  | expr_pr                   { $1 }
  | expr_uop expr_pr          { case ($1) of 
                                  AST.App a b -> AST.App a (b++[$2])
                                  e           -> AST.App e [$2]
                              }

expr_mul :: { AST.AST Name }
  : expr_mul bop_mul expr_uop { AST.Builtin $2 [$1,$3] } 
  | expr_uop                  { $1 }

expr_add :: { AST.AST Name }
  : expr_add bop_add expr_mul { AST.Builtin $2 [$1,$3] } 
  | expr_mul                  { $1 }

expr_cons :: { AST.AST Name }
  : expr_add ':' expr_cons     { AST.Cons $1 $3 } 
  | expr_add                   { $1 }

expr_cmp :: { AST.AST Name }
  : expr_cmp bop_cmp expr_cons { AST.Builtin $2 [$1,$3] } 
  | expr_cons                  { $1 }

expr_eq :: { AST.AST Name }
  : expr_eq bop_eq expr_cmp    { AST.Builtin $2 [$1,$3] } 
  | expr_cmp                   { $1 }

expr_and :: { AST.AST Name }
  : expr_and '&&' expr_eq      { AST.Builtin AST.BAnd [$1,$3] } 
  | expr_eq                    { $1 }

expr_or :: { AST.AST Name }
  : expr_or '||' expr_and      { AST.Builtin AST.BAnd [$1,$3] } 
  | expr_and                   { $1 }
  
expr :: { AST.AST Name }
  : expr_or                     { $1 }
  | if expr then expr else expr { AST.Ifte $2 $4 $6 }
  | fn id id_s '->' expr        { AST.Abs    ($2:$3) $5 }
  | let ldecl ldecl_s in expr   { AST.Let    ($2:$3) $5 }
  | letrec rdecl rdecl_s in expr{ AST.LetRec ($2:$3) $5 }
  | case expr of 
      '[' ']'   '->' expr ';' 
      id ':' id '->' expr       { AST.Case $2 $7 $9 $11 $13 }

  
uop :: { AST.Builtin }
  : '!'     { AST.UNot }
  | neg     { AST.UNeg }

bop_add :: { AST.Builtin }
  : '+'     { AST.BAdd }
  | '-'     { AST.BSub }

bop_mul :: { AST.Builtin }  
  : '*'     { AST.BMul }
  | '/'     { AST.BDiv }
  | '%'     { AST.BMod }

bop_cmp :: { AST.Builtin }
  : '<'     { AST.BLt  }
  | '<='    { AST.BLe  }
  | '>='    { AST.BGe  }
  | '>'     { AST.BGt  }

bop_eq :: { AST.Builtin }
  : '=='    { AST.BEq  }
  | '/='    { AST.BNe  }
    
{

type Name = String


happyError ([]) = error "unexpected end of file"
happyError ((x,(_,l,c),f):_) = error (show f ++ ":" ++ show l ++ ":" ++ show c ++ ": parser error near " ++ show x ++"\n")

parseExpr :: [(Token,Pos,FilePath)] -> AST.AST Name
parseExpr = expr

parseFile :: [(Token,Pos,FilePath)] -> [AST.Binding Name]
parseFile = parse

}
