module AST
  (Name, Literal, AST(..), Builtin(..))
  where

type Name = String

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

-- f x1 .. xn = e
type Binding = (Name, [Name], AST)

-- Abstract syntax tree for PuF
-- i don't really like the idea to have 2
-- constructors here for both let and letrec,
-- so i currently use Bool to flag if the
-- let is recursive or not (Bool should be replaced)
data AST
  = Var Name
  | Lit Literal
  | Ifte AST AST AST
  | Abs [Name] AST
  | App AST [AST]
  | Let Bool [Binding] AST
  | Builtin Builtin [AST]
