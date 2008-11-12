{
module PufParser (parseFile, parseExpr) where

import PufTokens
import qualified AST
import qualified Data.ByteString.Lazy.Char8 as ByteString

}

%name expr expr
%name parse parse
%tokentype { (Token,Pos,FilePath)}

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
  '>'      { (Bi BGt       ,_,_)}
  '>='     { (Bi BGe       ,_,_)}
  fn       { (Fn           ,_,_)}
  if       { (If           ,_,_)}
  then     { (Then         ,_,_)}
  else     { (Else         ,_,_)}
  mapsto   { (MapsTo       ,_,_)}
  let      { (Let          ,_,_)}
  letrec   { (Letrec       ,_,_)}
  in       { (In           ,_,_)}
  '='      { (Eq           ,_,_)}
  ','      { (Comma        ,_,_)}
  ';'      { (Semicolon    ,_,_)}
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
  
rdecl :: { (Name, AST.AST Name) }
  : id flhs '=' expr ';' { ($1, case $2 of [] -> $4 ; xs -> AST.Abs xs $4) }
  
ldecl :: { (Name, AST.AST Name) }
  : id flhs '=' expr ';' { ($1, case $2 of [] -> $4 ; xs -> AST.Abs xs $4) }
  | tlhs '=' expr ';' { (head $1, case tail $1 of [] -> $3 ; xs -> AST.Abs xs $3) }

ldecl_s :: { [AST.Binding Name] }
  :               {         [] }
  | ldecl_s ldecl { $1 ++ [$2] }

flhs :: { [Name] }
  :           {         [] }
  | flhs id   { $1 ++ [$2] }

id_list :: { [Name] }
  :                 {        [] }
  | id_list ',' id  { $1 ++ [$3] }

id_s :: { [Name] }
  :             {         [] }
  | id_list id  { $1 ++ [$2] }
  
tlhs :: { [Name] }
  : '(' id ',' id_list ')' { $2 : $4 }

expr_list :: { [AST.AST Name] }
  :                     {         [] }
  | expr_list ',' expr  { $1 ++ [$3] }

pr_expr :: { AST.AST Name}
  :  lit                                { AST.Lit $1 }
  |  id                                 { AST.Var $1 }
  | '(' ')'                             { AST.Tuple []}   
  | '(' expr ')'                        { $2 }
  | '(' expr_list ',' expr ',' expr ')' { AST.Tuple ($2 ++ [$4,$6])}   
  | '[' expr_list ']'                   { AST.List $2 }  
  
  
expr :: { AST.AST Name }
  : pr_expr                     { $1 }
  | uop expr                    { AST.Builtin $1 [$2] }
  | expr bop pr_expr            { AST.Builtin $2 [$1,$3] }
  | if expr then expr else expr { AST.Ifte $2 $4 $6 }
  | fn id id_s mapsto expr      { AST.Abs       ($2:$3) $5 }
  | let ldecl ldecl_s in expr   { AST.Let AST.NonRec ($2:$3) $5 }
  | letrec ldecl ldecl_s in expr{ AST.Let AST.Rec    ($2:$3) $5 }
  | expr pr_expr                { case ($1) of 
                                    AST.App a b -> AST.App a (b++[$2])
                                    e           -> AST.App e [$2]
                                }
  
uop :: { AST.Builtin }
  : '!'     { AST.UNot }
  | neg     { AST.UNeg }
  | select  { AST.USel $1 }

bop :: { AST.Builtin }
  : '+'     { AST.BAdd }
  | '-'     { AST.BSub }
  | '*'     { AST.BMul }
  | '/'     { AST.BDiv }
  | '%'     { AST.BMod }
  | '<'     { AST.BLt  }
  | '<='    { AST.BLe  }
  | '=='    { AST.BEq  }
  | '>='    { AST.BGe  }
  | '>'     { AST.BGt  }
  
select :: { Int }
  : '#' lit   { $2 }
  

{

type Name = String


happyError ([]) = error "unexpected end of file"
happyError ((x,(_,l,c),f):_) = error (show f ++ ":" ++ show l ++ ":" ++ show c ++ ": parser error near " ++ show x ++"\n")

parseExpr :: [(Token,Pos,FilePath)] -> AST.AST Name
parseExpr = expr

parseFile :: [(Token,Pos,FilePath)] -> [AST.Binding Name]
parseFile = parse

}
