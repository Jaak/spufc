module MaMa (MaMa(..), Label, allLabels) where

-- this will need to be changed
-- as we need unique label supply
-- type Label = Int ?
newtype Label = Label Int
  deriving (Eq, Ord)

allLabels :: [Label]
allLabels = map Label [0..]

data MaMa
  = MKBASIC
  | GETBASIC
  | EVAL
  | ADD
  | SUB
  | MUL
  | DIV
  | LEQ
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
  | LABEL Label
  | MKCLOS Label
  | MKFUNVAL Label
  | MARK Label
  | JUMPZ Label
  | JUMP Label

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
showsMama (APPLY)      = showString "apply"
showsMama (UPDATE)     = showString "update"
showsMama (HALT)       = showString "halt"
showsMama (LOADC k)    = showString "loadc"    . space . shows k
showsMama (PUSHLOC k)  = showString "pushloc"  . space . shows k
showsMama (PUSHGLOB k) = showString "pushglob" . space . shows k
showsMama (TARG k)     = showString "targ"     . space . shows k
showsMama (RETURN k)   = showString "return"   . space . shows k 
showsMama (SLIDE k)    = showString "slide"    . space . shows k
showsMama (ALLOC k)    = showString "alloc"    . space . shows k
showsMama (REWRITE k)  = showString "rewrite"  . space . shows k 
showsMama (MKVEC k)    = showString "mkvec"    . space . shows k
showsMama (LABEL l)    = shows l               . showChar ':'
showsMama (MKCLOS l)   = showString "mkclos"   . space . shows l
showsMama (MKFUNVAL l) = showString "mkfunval" . space . shows l
showsMama (MARK l)     = showString "mark"     . space . shows l
showsMama (JUMPZ l)    = showString "jumpz"    . space . shows l
showsMama (JUMP l)     = showString "jump"     . space . shows l
