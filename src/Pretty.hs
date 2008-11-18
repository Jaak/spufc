module Pretty (prettyAST) where

import Text.PrettyPrint.HughesPJ

import AST

tuple :: [Doc] -> Doc
tuple = parens . hsep . punctuate comma

prettyAST :: Show a => AST a -> String
prettyAST = render . prettyAST'

prettyAST' (Var x) = text (show x)
prettyAST' (Lit n) = int n
prettyAST' (Ifte e t f) =
  (text "if" <+> prettyAST' e) $$ nest 2 (
    (text "then" <+> prettyAST' t $$
     text "else" <+> prettyAST' f))
prettyAST' (Abs ns e) =
  text "fn" <+> hsep (map (text . show) ns) <+> text "->" <+> prettyAST' e
prettyAST' (App e es) =
  prettyApp e <+> hsep (map prettyPrim es)
prettyAST' (LetRec bs e) =
  text "letrec" $$
  nest 2 (vcat $ map prettyBind bs) $$
  text "in" <+>
  prettyAST' e
prettyAST' (Let bs e) =
  text "let" $$
  nest 2 (vcat $ map prettyDecl bs) $$
  text "in" <+>
  prettyAST' e
prettyAST' (Builtin bi es) = prettyBuiltin bi (map prettyPrim es)
prettyAST' (MkTuple es) = tuple (map prettyAST' es)
prettyAST' (Select i e) = char '#' <> int i <> prettyPrim e
prettyAST' Nil = text "[]"
prettyAST' (Cons e es) = prettyPrim e <+> colon <+> prettyCons es
prettyAST' (Case cbody cnil xh xt ccons) =
  (text "case" <+> prettyAST' cbody <+> text "of") $$
    nest 2 (pNil $$ pCons)
  where
    pNil = 
      text "[]" <+> text "->" <+> prettyAST' cnil <> semi
    pCons =
      text (show xh) <+> colon <+> text (show xt) <+> text "->"
      <+> prettyAST' ccons

prettyCons (Cons e es) = prettyPrim e <+> colon <+> prettyCons es
prettyCons e = prettyPrim e

prettyBuiltin BAdd [p, p'] = p <+> text "+" <+> p'
prettyBuiltin BMul [p, p'] = p <+> text "*" <+> p'
prettyBuiltin BSub [p, p'] = p <+> text "-" <+> p'
prettyBuiltin BLe  [p, p'] = p <+> text "<=" <+> p'
prettyBuiltin BEq  [p, p'] = p <+> text "==" <+> p'
prettyBuiltin BDiv [p, p'] = p <+> text "/" <+> p' 
prettyBuiltin BMod [p, p'] = p <+> text "%" <+> p' 
prettyBuiltin BNe  [p, p'] = p <+> text "/=" <+> p' 
prettyBuiltin BLt  [p, p'] = p <+> text "<" <+> p' 
prettyBuiltin BGe  [p, p'] = p <+> text ">=" <+> p' 
prettyBuiltin BGt  [p, p'] = p <+> text ">" <+> p' 
prettyBuiltin UNeg [p]     = text "neg" <+> p
prettyBuiltin UNot [p]     = text "not" <+> p
prettyBuiltin bi ps = text (show bi) <+> hsep ps

prettyPrim (Var x) = text (show x)
prettyPrim (Lit n) = int n
prettyPrim e@Nil = prettyAST' e
prettyPrim e@(MkTuple _) = prettyAST' e
prettyPrim e = parens (prettyAST' e)

prettyApp e@(App _ _) = prettyAST' e
prettyApp e = prettyPrim e

prettyBind (b, e) =
  text (show b) <+> char '=' <+> prettyAST' e

prettyDecl (Single b e) = prettyBind (b, e)
prettyDecl (Tuple bs e) =
  tuple (map (text . show) bs) <+> char '=' <+> prettyAST' e
