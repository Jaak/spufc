module Type (TypeVar, mkTypeVar, Type(..)) where

import Unique

newtype TypeVar = TypeVar Unique

mkTypeVar :: Unique -> TypeVar
mkTypeVar = TypeVar

instance Eq TypeVar where
  (TypeVar n) == (TypeVar m) = n == m

instance Ord TypeVar where
  (TypeVar n) `compare` (TypeVar m) = n `compare` m

data Type
  = TyVar TypeVar
  | TyFun Type Type
  | TyProd [Type]
  | TyInt
  | TyBool
