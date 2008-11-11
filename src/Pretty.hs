module Pretty (prettyAST) where

import Text.PrettyPrint.HughesPJ

import AST

prettyAST :: Show a => AST a -> String
prettyAST = render . prettyAST'

prettyAST' (Var x) = text (show x)
prettyAST' (Lit n) = int n
prettyAST' (Ifte e t f) =
  (text "if" <+> prettyAST' e) $$ nest 2 (
    (text "then" <+> prettyAST' t $$
     text "else" <+> prettyAST' f))
prettyAST' (Abs ns e) =
  char '\\' <> hsep (map (text . show) ns) <+> text "->" <+> prettyAST' e
prettyAST' (App e es) =
  prettyApp e <+> hsep (map prettyPrim es)
prettyAST' (Let rec bs e) =
  prettyRec rec $$
  nest 2 (vcat $ map prettyBind bs) $$
  text "in" <+>
  prettyAST' e
prettyAST' (Builtin bi es) = prettyBuiltin bi (map prettyPrim es)

prettyRec Rec = text "let" <+> text "rec"
prettyRec NonRec = text "let"

prettyBuiltin BAdd [p, p'] = p <+> text "+" <+> p'
prettyBuiltin BMul [p, p'] = p <+> text "*" <+> p'
prettyBuiltin BSub [p, p'] = p <+> text "-" <+> p'
prettyBuiltin BLe [p, p'] = p <+> text "<=" <+> p'
prettyBuiltin BEq [p, p'] = p <+> text "==" <+> p'
prettyBuiltin bi ps = text (show bi) <+> hsep ps

prettyPrim (Var x) = text (show x)
prettyPrim (Lit n) = int n
prettyPrim e = parens (prettyAST' e)

prettyApp e@(App _ _) = prettyAST' e
prettyApp e = prettyPrim e

prettyBind (b, e) =
  text (show b) <+> char '=' <+> prettyAST' e
