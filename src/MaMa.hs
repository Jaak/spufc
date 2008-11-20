module MaMa (MaMa(..), Label, mkLabel) where

import Unique

newtype Label = Label Unique
  deriving (Eq, Ord)

mkLabel :: Unique -> Label
mkLabel = Label

data MaMa
  = MKBASIC
  | GETBASIC
  | EVAL
  | ADD
  | SUB
  | MUL
  | DIV
  | LEQ
  | NEG
  | NOT
  | MOD
  | NEQ
  | LE
  | GEQ
  | GR
  | OR
  | AND
  | EQUAL
  | APPLY
  | UPDATE
  | HALT
  | LOADC Int
  | PUSHLOC Int
  | PUSHGLOB Int
  | TARG Int
  | RETURN Int
  | SLIDE Int
  | ALLOC Int
  | REWRITE Int
  | MKVEC Int
  | GETVEC Int
  | GET Int
  | LABEL Label
  | MKCLOS Label
  | MKFUNVAL Label
  | MARK Label
  | JUMPZ Label
  | JUMP Label
  | COMMENT String

instance Show MaMa where showsPrec _ = showsMama
instance Show Label where showsPrec _ = showsLabel

showsLabel :: Label -> ShowS
showsLabel (Label l) = showChar '_' . shows l

space :: ShowS
space = showChar ' '

showsMama :: MaMa -> ShowS
showsMama (MKBASIC)    = showString "mkbasic"
showsMama (GETBASIC)   = showString "getbasic"
showsMama (EVAL)       = showString "eval"
showsMama (ADD)        = showString "add"
showsMama (SUB)        = showString "sub"
showsMama (MUL)        = showString "mul"
showsMama (DIV)        = showString "div"
showsMama (LEQ)        = showString "leq"
showsMama (EQUAL)      = showString "eq"
showsMama (APPLY)      = showString "apply"
showsMama (UPDATE)     = showString "update"
showsMama (HALT)       = showString "halt"
showsMama (NEG)        = showString "neg" 
showsMama (NOT)        = showString "not" 
showsMama (MOD)        = showString "mod" 
showsMama (NEQ)        = showString "neq" 
showsMama (LE)         = showString "le" 
showsMama (GEQ)        = showString "geq" 
showsMama (GR)         = showString "gr" 
showsMama (OR)         = showString "or" 
showsMama (AND)        = showString "and" 
showsMama (LOADC k)    = showString "loadc"    . space . shows k
showsMama (PUSHLOC k)  = showString "pushloc"  . space . shows k
showsMama (PUSHGLOB k) = showString "pushglob" . space . shows k
showsMama (TARG k)     = showString "targ"     . space . shows k
showsMama (RETURN k)   = showString "return"   . space . shows k 
showsMama (SLIDE k)    = showString "slide"    . space . shows k
showsMama (ALLOC k)    = showString "alloc"    . space . shows k
showsMama (REWRITE k)  = showString "rewrite"  . space . shows k 
showsMama (MKVEC k)    = showString "mkvec"    . space . shows k
showsMama (GETVEC k)   = showString "getvec"   . space . shows k
showsMama (GET k)      = showString "get"      . space . shows k
showsMama (LABEL l)    = shows l               . showChar ':'
showsMama (MKCLOS l)   = showString "mkclos"   . space . shows l
showsMama (MKFUNVAL l) = showString "mkfunval" . space . shows l
showsMama (MARK l)     = showString "mark"     . space . shows l
showsMama (JUMPZ l)    = showString "jumpz"    . space . shows l
showsMama (JUMP l)     = showString "jump"     . space . shows l
showsMama (COMMENT str)= showString "/* " . showString str . showString " */"
