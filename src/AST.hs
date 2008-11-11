module AST
  (Literal, AST(..), Builtin(..), RecOrNot(..), Binding)
  where

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
  | USel Int 
  deriving (Eq,Show)

-- f = e
type Binding a = (a, AST a)

data RecOrNot = Rec | NonRec
  deriving (Eq, Show)

data AST a
  = Var a
  | Lit Literal
  | Tuple [AST a]
  | List [AST a]
  | Ifte (AST a) (AST a) (AST a)
  | Abs [a] (AST a)
  | App (AST a) [AST a]
  | Let RecOrNot [Binding a] (AST a)
  | Builtin Builtin [AST a]
  deriving (Eq,Show)
