import Pretty
import CodeGen
import AST

--
-- We have tiny dsl to simplify writing expressions:

infix 0 .=
(.=) = (,)
infixl 1 `app`
app = App

instance Num AST where
  fromInteger = Lit . fromInteger
  x + y = Builtin BAdd [x, y]
  x - y = Builtin BSub [x, y]
  x * y = Builtin BMul [x, y]
  abs = undefined
  signum = undefined

--
-- some tests:

t 1 = Let False [
    "a" .= 19,
    "b" .= Var "a" * Var "a"]
  (Var "a" + Var "b")

-- factorial of 8
t 2 = Let True [
    "f" .= Abs ["x", "y"] (
      Ifte (Builtin BLe [Var "y", 1])
        (Var "x")
        (Var "f" `app` [
          Var "x" * Var "y",
          Var "y" - 1])
    )]
  (Var "f" `app` [1, 8])

-- odd / even
t 3 = Let True [
    "odd" .= (Abs ["x"] (
        Ifte (Builtin BEq [Var "x", 0])
             0
             (Var "even" `app` [Var "x" - 1])
      )),
    "even" .= (Abs ["x"] (
        Ifte (Builtin BEq [Var "x", 0])
             1
             (Var "odd" `app` [Var "x" - 1])
      ))]
  (Var "even" `app` [11])

--
t 4 = Let False [
    "add" .= Abs ["x", "y"] (Var "x" + Var "y"),
    "mul" .= Abs ["x", "y"] (Var "x" * Var "y")]
  (Var "add" `app` [10, Var "mul" `app` [10, 11]])

-- partial application
t 5 = Let True [
    "add" .= Abs ["x", "y"] (Var "x" + Var "y")]
  (Var "add" `app` [10] `app` [11])

--
t 6 = Let True [
    "one" .= Abs ["x"] (App (Var "two") [Var "x"]),
    "two" .= Abs ["x"] 2]
  (Var "one" `app` [1])

-- infinite loop
t 7 = Let True [
    "a" .= Var "b",
    "b" .= Var"a"]
  (Var "a")

-- stupid Y combinator
t 8 = Let True [
    "fix" .= Abs ["f"] (Var "f" `app` [Var "fix" `app` [Var "f"]]),
    "step" .= Abs ["f", "x"] (Ifte (Builtin BEq [Var "x", 1])
      (Var "x")
      (Builtin BMul [Var "x", Var "f" `app` [Builtin BSub [Var "x", 1]]]))]
  ((Var "fix" `app` [Var "step"]) `app` [5])

-- ncier Y combinator
t 9 = Let False [
    "fix" .= Abs ["f"] (Let True ["x" .= Var "f" `app` [Var "x"]] (Var "x")),
    "step" .= Abs ["f", "x"] (Ifte (Builtin BEq [Var "x", 1])
      (Var "x")
      (Builtin BMul [Var "x", Var "f" `app` [Builtin BSub [Var "x", 1]]]))]
  ((Var "fix" `app` [Var "step"]) `app` [5])
