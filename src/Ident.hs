module Ident
  (Ident, mkIdentFrom, mkIdent, identName)
  where

import Unique
import Pretty

data Ident = Ident {
    uniq :: Unique,
    identName :: String
  }

instance Eq Ident where
  id == id' = uniq id == uniq id'

instance Ord Ident where
  compare id id' = compare (uniq id) (uniq id')

instance Show Ident where
  showsPrec n id = showString (identName id)

instance Pretty Ident where
  pprint id = pprint (identName id)

mkIdent :: Unique -> Ident
mkIdent = mkIdentFrom "_"

mkIdentFrom :: String -> Unique -> Ident
mkIdentFrom name u = Ident u name
