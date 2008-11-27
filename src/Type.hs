module Type (TypeVar, mkTypeVar, Type(..)) where

import Pretty
import Unique

newtype TypeVar = TypeVar Unique

mkTypeVar :: Unique -> TypeVar
mkTypeVar = TypeVar

instance Eq TypeVar where
  (TypeVar n) == (TypeVar m) = n == m

instance Ord TypeVar where
  (TypeVar n) `compare` (TypeVar m) = n `compare` m

instance Pretty TypeVar where
  pprint (TypeVar v) = text "a" <> pprint v

data Type
  = TVar TypeVar
  | TFun Type Type
  | TProd [Type]
  | TInt

instance Pretty Type where
  pprint (TVar tv) = pprint tv
  pprint (TFun t t') = prettyFun t <+> text "->" <+> pprint t'
    where
      prettyFun t@(TFun _ _) = parens (pprint t)
      prettyFun t = pprint t
  pprint (TProd ts) = parens (hsep (punctuate comma (map pprint ts)))
  pprint TInt = text "Int"
