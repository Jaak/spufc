{
module PufParser (parseFile, parseExpr) where

import PufTokens
import qualified AST
import qualified Data.ByteString.Lazy.Char8 as ByteString

}

%name expr expr
%name parse parse
%tokentype { Token }

%token
  lit      { Lit $$ }
  id       { Id  $$ }
  '#'      { Sel    }
  '!'      { Bi UNot }
  neg      { Bi UNeg }
  '+'      { Bi BAdd }
  '-'      { Bi BSub }
  '*'      { Bi BMul }
  '/'      { Bi BDiv }
  '%'      { Bi BMod }
  '<'      { Bi BLt }
  '<='     { Bi BLe }
  '=='     { Bi BEq }
  '>'      { Bi BGt }
  '>='     { Bi BGe }
  fn       { Fn }
  if       { If }
  then     { Then }
  else     { Else }
  mapsto   { MapsTo }
  let      { Let  }
  letrec   { Letrec }
  in       { In }
  '='      { Eq }
  ','      { Comma }
  ';'      { Semicolon }
  '('      { ParenLeft }
  ')'      { ParenRight }
  '['      { BracketLeft }
  ']'      { BracketRight }
  err      { Error $$ }
%%

parse :: { [AST.Binding Name] }
  : program  { $1 } 

program :: { [AST.Binding Name] }
  :                   {         [] }
  | program rdecl     { $1 ++ [$2] }
  
rdecl :: { (Name, AST.AST Name) }
  : flhs '=' expr ';' { (head $1, case tail $1 of [] -> $3 ; xs -> AST.Abs xs $3) }
  
ldecl :: { (Name, AST.AST Name) }
  : flhs '=' expr ';' { (head $1, case tail $1 of [] -> $3 ; xs -> AST.Abs xs $3) }
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
  | let ldecl_s in expr         { AST.Let AST.NonRec $2 $4 }
  | letrec ldecl_s in expr      { AST.Let AST.Rec    $2 $4 }
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

happyError x = error ("Parse error: " ++ show  x ++ "\n")

parseExpr :: [Token] -> AST.AST Name
parseExpr = expr

parseFile ::  [Token] -> [AST.Binding Name]
parseFile = parse

}
