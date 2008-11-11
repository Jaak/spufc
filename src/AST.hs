module AST
  (Literal, AST(..), Builtin(..), RecOrNot(..))
  where

type Literal = Int

-- unary and binary operators
-- i don't see any good reason to separate those 2
data Builtin
  = UNeg
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
  deriving (Eq,Show)

-- f = e
type Binding a = (a, AST a)

data RecOrNot = Rec | NonRec
  deriving (Eq, Show)

-- Abstract syntax tree for PuF
-- i don't really like the idea to have 2
-- constructors here for both let and letrec,
-- so i currently use Bool to flag if the
-- let is recursive or not (Bool should be replaced)
data AST a
  = Var a
  | Lit Literal
  | Ifte (AST a) (AST a) (AST a)
  | Abs [a] (AST a)
  | App (AST a) [AST a]
  | Let RecOrNot [Binding a] (AST a)
  | Builtin Builtin [AST a]
  deriving (Eq,Show)
