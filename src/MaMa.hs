module MaMa (MaMa(..), Label) where

-- this will need to be changed
-- as we need unique label supply
-- type Label = Int ?
type Label = String

data MaMa
  = MKBASIC
  | GETBASIC
  | EVAL
  | ADD
  | SUB
  | MUL
  | DIV
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
  | MARK Label
  | JUMPZ Label
  | JUMP Label

instance Show MaMa where
  showsPrec _ = showsMama

showsMama :: MaMa -> ShowS
showsMama (MKBASIC)      = showString "mkbasic"
showsMama (GETBASIC)     = showString "getbasic"
showsMama (EVAL)         = showString "eval"
showsMama (ADD)          = showString "add"
showsMama (SUB)          = showString "sub"
showsMama (MUL)          = showString "mul"
showsMama (DIV)          = showString "div"
showsMama (APPLY)        = showString "apply"
showsMama (UPDATE)       = showString "update"
showsMama (HALT)         = showString "halt"
showsMama (LOADC k)      = showString "loadc"    . showChar ' ' . shows k
showsMama (PUSHLOC k)    = showString "pushloc"  . showChar ' ' . shows k
showsMama (PUSHGLOB k)   = showString "pushglob" . showChar ' ' . shows k
showsMama (TARG k)       = showString "targ"     . showChar ' ' . shows k
showsMama (RETURN k)     = showString "return"   . showChar ' ' . shows k 
showsMama (SLIDE k)      = showString "slide"    . showChar ' ' . shows k
showsMama (ALLOC k)      = showString "alloc"    . showChar ' ' . shows k
showsMama (REWRITE k)    = showString "rewrite"  . showChar ' ' . shows k 
showsMama (MKVEC k)      = showString "mkvec"    . showChar ' ' . shows k
showsMama (LABEL label)  = showString label      . showChar ':'
showsMama (MKCLOS label) = showString "mkclos"   . showChar ' ' . showString label
showsMama (MARK label)   = showString "mark"     . showChar ' ' . showString label
showsMama (JUMPZ label)  = showString "jumpz"    . showChar ' ' . showString label
showsMama (JUMP label)   = showString "jump"     . showChar ' ' . showString label
