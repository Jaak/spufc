module AST
  (Literal, AST(..), Builtin(..), Decl(..), Binding)
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
  | BOr
  | BAnd
  deriving (Eq,Show)

type Binding a = (a, AST a)

data Decl a
  = Tuple [a] (AST a)
  | Single a (AST a)
  deriving (Eq,Show)

data AST a
  = Var a
  | Lit Literal
  | Ifte (AST a) (AST a) (AST a)
  | Abs [a] (AST a)
  | App (AST a) [AST a]
  | Let [Decl a] (AST a)
  | LetRec [Binding a] (AST a)
  | Builtin Builtin [AST a]
  -- tuples
  | MkTuple [AST a]
  | Select Int (AST a)
  -- lists
  | Nil
  | Cons (AST a) (AST a)
  | Case (AST a) (AST a) a a (AST a)
  deriving (Eq,Show)
