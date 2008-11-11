{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

import Control.Arrow (left, right)
import Data.String

import Pretty
import CodeGen
import AST

--
-- We have tiny dsl to simplify writing expressions:

infix 0 .=
(.=) = (,)
infixl 1 `app`
app = App


instance (Show a, Eq a) => Num (AST a) where
  fromInteger = Lit . fromInteger
  x + y = Builtin BAdd [x, y]
  x - y = Builtin BSub [x, y]
  x * y = Builtin BMul [x, y]
  abs = undefined
  signum = undefined

instance IsString (AST String) where
  fromString = Var

--
-- some tests:

t 1 = Let NonRec [
    "a" .= 19,
    "b" .= "a" * "a"]
  ("a" + "b")

-- factorial of 8
t 2 = Let Rec [
    "f" .= Abs ["x", "y"] (
      Ifte (Builtin BLe [Var "y", 1])
        "x"
        ("f" `app` ["x" * "y", "y" - 1])
    )]
  ("f" `app` [1, 8])

-- odd / even
t 3 = Let Rec [
    "odd" .= (Abs ["x"] (
        Ifte (Builtin BEq ["x", 0])
             0
             ("even" `app` ["x" - 1])
      )),
    "even" .= (Abs ["x"] (
        Ifte (Builtin BEq ["x", 0])
             1
             ("odd" `app` ["x" - 1])
      ))]
  ("even" `app` [11])

--
t 4 = Let NonRec [
    "add" .= Abs ["x", "y"] ("x" + "y"),
    "mul" .= Abs ["x", "y"] ("x" * "y")]
  ("add" `app` [10, "mul" `app` [10, 11]])

-- partial application
t 5 = Let Rec [
    "add" .= Abs ["x", "y"] ("x" + "y")]
  ("add" `app` [10] `app` [11])

--
t 6 = Let Rec [
    "one" .= Abs ["x"] ("two" `app` ["x"]),
    "two" .= Abs ["y"] 2]
  ("one" `app` [1])

-- infinite loop
t 7 = Let Rec [
    "a" .= "b",
    "b" .= "a"]
  "a"

-- stupid Y combinator
t 8 = Let Rec [
    "fix" .= Abs ["f"] ("f" `app` ["fix" `app` ["f"]]),
    "step" .= Abs ["f", "x"] (Ifte (Builtin BEq ["x", 1])
      "x"
      ("x" * ("f" `app` ["x" - 1])))]
  (("fix" `app` ["step"]) `app` [5])

-- nicer Y combinator
t 9 = Let NonRec [
    "fix" .= Abs ["f"] (Let Rec ["x" .= "f" `app` ["x"]] "x"),
    "step" .= Abs ["f", "x"] (Ifte (Builtin BEq ["x", 1])
      "x"
      ("x" * ("f" `app` ["x" - 1])))]
  (("fix" `app` ["step"]) `app` [5])
