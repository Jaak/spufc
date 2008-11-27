module Ident
  (Ident, mkIdentFrom, mkIdent, identName,
   identType, identSetType)
  where

import Unique
import Type
import Pretty

data Ident = Ident {
    uniq :: Unique,
    identName :: String,
    identType :: Maybe Type
  }

instance Eq Ident where
  id == id' = uniq id == uniq id'

instance Ord Ident where
  compare id id' = compare (uniq id) (uniq id')

instance Show Ident where
  showsPrec n id = showString (identName id)

instance Pretty Ident where
  pprint id = case identType id of
    Nothing -> name
    Just ty -> parens (name <+> colon <> colon <+> pprint ty)
    where
      name = pprint (identName id)

identSetType :: Type -> Ident -> Ident
identSetType ty id = id { identType = Just ty }

mkIdent :: Unique -> Ident
mkIdent = mkIdentFrom "_"

mkIdentFrom :: String -> Unique -> Ident
mkIdentFrom name u = Ident u name Nothing
