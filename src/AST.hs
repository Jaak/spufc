module AST
  (Name, Literal, AST(..), Builtin(..), RecOrNot(..), Binding)
  where

type Name = String

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
type Binding = (Name, AST)

data RecOrNot = Rec | NonRec
  deriving (Eq, Show)

-- Abstract syntax tree for PuF
-- i don't really like the idea to have 2
-- constructors here for both let and letrec,
-- so i currently use Bool to flag if the
-- let is recursive or not (Bool should be replaced)
data AST
  = Var Name
  | Lit Literal
  | Tuple [AST]
  | List  [AST]
  | Ifte AST AST AST
  | Abs [Name] AST
  | App AST [AST]
  | Let RecOrNot [Binding] AST
  | Builtin Builtin [AST]
  deriving (Eq,Show)
