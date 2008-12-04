module AST
  (Literal, AST(..), Builtin(..), Bind(..), AppType(..),
   collectApp, collectAbs, collectLet,
   mkAbs, mkLet)
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

data Bind a
  = Single a (AST a)
  | Tuple [a] (AST a)
  | Rec [(a, AST a)]
  deriving (Eq,Show)

data AppType
  = RegularCall
  | LastCall Int
  deriving (Eq, Show)

data AST a
  = Var a
  | Lit Literal
  | Ifte (AST a) (AST a) (AST a)
  | Abs a (AST a)
  | App AppType (AST a) (AST a)
  | Let (Bind a) (AST a)
  | Builtin Builtin [AST a]
  -- tuples
  | MkTuple [AST a]
  | Select Int (AST a)
  -- lists
  | Nil
  | Cons (AST a) (AST a)
  | Case (AST a) (AST a) a a (AST a)
  deriving (Eq,Show)

mkAbs :: [a] -> AST a -> AST a
mkAbs xs e = foldr Abs e xs

collectAbs :: AST a -> ([a], AST a)
collectAbs (Abs x e) = let
    (xs, e') = collectAbs e
  in (x : xs, e')
collectAbs e = ([], e)

mkLet :: [Bind a] -> AST a -> AST a
mkLet bs e = foldr Let e bs

collectLet :: AST a -> ([Bind a], AST a)
collectLet (Let b@(Single _ _) e) = let
    (bs, e') = collectLet e
  in (b : bs, e')
collectLet (Let b@(Tuple _ _) e) = let
    (bs, e') = collectLet e
  in (b : bs, e')
collectLet e = ([], e)

collectApp :: AST a -> (AppType, AST a, [AST a])
collectApp  = loop [] RegularCall
  where
    loop args _ (App t e arg) = loop (arg : args) t e
    loop args t e = (t, e, args)

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
  pprint e@(Abs _ _) =
    text "fn" <+> hsep (map pprint xs) <+> text "->" <+> pprint e'
    where
      (xs, e') = collectAbs e
  pprint e@(App _ _ _) = case t of
    RegularCall ->
      prettyApp e' <+> hsep (map prettyPrim args)
    LastCall k ->
      braces (int k <> char '|' <+> prettyApp e' <+> hsep (map prettyPrim args))
    where
      (t, e', args) = collectApp e
  pprint (Let (Rec bs) e) =
    text "letrec" $$
    nest 2 (vcat $ map prettyBind bs) $$
    text "in" <+>
    pprint e
  pprint e@(Let _ _) =
    text "let" $$
    nest 2 (vcat $ map prettyDecl bs) $$
    text "in" <+>
    pprint e'
    where
      (bs, e') = collectLet e
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
