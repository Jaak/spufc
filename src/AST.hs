module AST
  (Literal, AST(..), Builtin(..), Decl(..), Binding, AppType(..))
  where

import Pretty

type Literal = Int

-- unary and binary operators
-- i don't see any good reason to separate those 2
data Builtin
  = UNeg
  | UNot
  | BAdd
  | BSub
  | BMul
  | BDiv
  | BMod
  | BEq
  | BNe
  | BLe
  | BLt
  | BGe
  | BGt
  | BOr
  | BAnd
  deriving (Eq,Show)

type Binding a = (a, AST a)

data Decl a
  = Tuple [a] (AST a)
  | Single a (AST a)
  deriving (Eq,Show)

data AppType
  = RegularCall
  | LastCall Int
  deriving (Eq, Show)

data AST a
  = Var a
  | Lit Literal
  | Ifte (AST a) (AST a) (AST a)
  | Abs [a] (AST a)
  | App AppType (AST a) [AST a]
  | Let [Decl a] (AST a)
  | LetRec [Binding a] (AST a)
  | Builtin Builtin [AST a]
  -- tuples
  | MkTuple [AST a]
  | Select Int (AST a)
  -- lists
  | Nil
  | Cons (AST a) (AST a)
  | Case (AST a) (AST a) a a (AST a)
  deriving (Eq,Show)


--
-- Pretty printing
--

instance Pretty a => Pretty (AST a) where
  pprint (Var x) = pprint x
  pprint (Lit n) = pprint n
  pprint  (Ifte e t f) =
    (text "if" <+> pprint e) $$ nest 2 (
      (text "then" <+> pprint t $$
       text "else" <+> pprint f))
  pprint (Abs xs e) =
    text "fn" <+> hsep (map pprint xs) <+> text "->" <+> pprint e
  pprint (App RegularCall e es) =
    prettyApp e <+> hsep (map prettyPrim es)
  pprint (App (LastCall k) e es) =
    braces (int k <> char '|' <+> prettyApp e <+> hsep (map prettyPrim es))
  pprint (LetRec bs e) =
    text "letrec" $$
    nest 2 (vcat $ map prettyBind bs) $$
    text "in" <+>
    pprint e
  pprint (Let bs e) =
    text "let" $$
    nest 2 (vcat $ map prettyDecl bs) $$
    text "in" <+>
    pprint e
  pprint (Builtin bi es) = prettyBuiltin bi (map prettyPrim es)
  pprint (MkTuple es) = tuple (map pprint es)
  pprint (Select i e) = char '#' <> int i <> prettyPrim e
  pprint Nil = text "[]"
  pprint (Cons e es) = prettyPrim e <+> colon <+> prettyCons es
  pprint (Case cbody cnil xh xt ccons) =
    (text "case" <+> pprint cbody <+> text "of") $$
      nest 2 (pNil $$ pCons)
    where
      pNil = text "[]" <+> text "->" <+> pprint cnil
      pCons =
        pprint xh <+> colon <+> pprint xt <+> text "->"
        <+> pprint ccons

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

prettyPrim (Var x) = pprint x
prettyPrim (Lit n) = int n
prettyPrim e@Nil = pprint e
prettyPrim e@(MkTuple _) = pprint e
prettyPrim e = parens (pprint e)

prettyApp e@(App _ _ _) = pprint e
prettyApp e = prettyPrim e

prettyBind (b, e) =
  pprint b <+> char '=' <+> pprint e

prettyDecl (Single b e) = prettyBind (b, e)
prettyDecl (Tuple bs e) =
  tuple (map pprint bs) <+> char '=' <+> pprint e

tuple :: [Doc] -> Doc
tuple = parens . hsep . punctuate comma
