module Ident
  (Ident, mkIdentFrom, mkIdent, identName)
  where

import Unique

data Ident = Ident String Unique

instance Eq Ident where Ident _ n == Ident _ m = n == m
instance Ord Ident where compare (Ident _ n) (Ident _ m) = compare n m

instance Show Ident where
  showsPrec n (Ident name k) = showString name

identName :: Ident -> String
identName (Ident name _) = name

mkIdent :: Unique -> Ident
mkIdent = mkIdentFrom "_"

mkIdentFrom :: String -> Unique -> Ident
mkIdentFrom = Ident
