module Pretty (prettyAST) where

import Text.PrettyPrint.HughesPJ

import AST

prettyAST = render . prettyAST'

prettyAST' (Var x) = text x
prettyAST' (Lit n) = int n
prettyAST' (Ifte e t f) =
  (text "if" <+> prettyAST' e) $$ nest 2 (
    (text "then" <+> prettyAST' t $$
     text "else" <+> prettyAST' f))
prettyAST' (Abs ns e) =
  char '\\' <> hsep (map text ns) <+> text "->" <+> prettyAST' e
prettyAST' (App e es) =
  prettyApp e <+> hsep (map prettyPrim es)
prettyAST' (Let _ bs e) =
  text "let" $$
  nest 2 (vcat $ map prettyBind bs) $$
  text "in" <+>
  prettyAST' e
prettyAST' (Builtin bi es) = prettyBuiltin bi (map prettyPrim es)

prettyBuiltin BAdd [p, p'] = p <+> text "+" <+> p'
prettyBuiltin BMul [p, p'] = p <+> text "*" <+> p'
prettyBuiltin BSub [p, p'] = p <+> text "-" <+> p'
prettyBuiltin BLe [p, p'] = p <+> text "<=" <+> p'
prettyBuiltin BEq [p, p'] = p <+> text "==" <+> p'
prettyBuiltin bi ps = text (show bi) <+> hsep ps

prettyPrim (Var x) = text x
prettyPrim (Lit n) = int n
prettyPrim e = parens (prettyAST' e)

prettyApp e@(App _ _) = prettyAST' e
prettyApp e = prettyPrim e

prettyBind (b, ns, e) =
  text b <+> hsep (map text ns) <+> char '=' <+> prettyAST' e
