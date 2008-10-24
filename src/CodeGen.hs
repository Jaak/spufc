module CodeGen (codeGen) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader

import CgMonad
import AST
import MaMa
import Env as Env

-- each builtin must correspond to some MaMa instruction
-- well we could generalise this with:
--  builtin :: Builtin -> [MaMa]
builtin :: Builtin -> MaMa
builtin = undefined

codeGen :: AST -> [MaMa]
codeGen e = runCg $ do
  codev e
  emit HALT

codev, codeb, codec  :: AST -> Cg ()

-- simple expressions
codeb (Lit x) = emit (LOADC x)
codeb (Builtin bi es) = do
  k <- asks sd
  zipWithM  (\sd' -> withSd sd' . codeb) [k ..] es
  emit (builtin bi)
codeb (Ifte e t f) = do
  a <- newLabel
  b <- newLabel
  codeb e
  emit (JUMPZ a)
  codeb t
  emit (JUMP b)
  emit (LABEL a)
  codeb f
  emit (LABEL b)
codeb e = do
  codev e
  emit GETBASIC

codev (Lit x) = emits [LOADC x, MKBASIC]
codev (Builtin bi es) = do
  sd <- asks sd
  zipWithM  (\sd' -> withSd sd' . codeb) [sd ..] es
  emit (builtin bi)
  emit MKBASIC
codev (Ifte e t f) = do
  a <- newLabel
  b <- newLabel
  codeb e
  emit (JUMPZ a)
  codev t
  emit (JUMP b)
  emit (LABEL a)
  codev f
  emit (LABEL b)
codev (Var x) = do
  getvar x
  emit EVAL
codev (Abs xs e) = do
  a <- newLabel
  b <- newLabel
  sd <- asks sd
  let
    k = length xs
    d = length zs
    zs = undefined
    env' = Env.fromList $
             zip xs [Local (-i) | i <- [0..]] ++
             zip zs [Global i   | i <- [0..]]
  zipWithM (\sd' -> withSd k . getvar) [sd ..] zs
  emits [
    MKVEC d,
    MKFUNVAL a,
    JUMP b,
    LABEL a,
    TARG k]
  withSd 0 (withEnv env' (codev e))
  emits [RETURN k, LABEL b]

codec = undefined

getvar :: Name -> Cg ()
getvar x = do
  env <- asks env
  case Env.lookup env x of
    Local i -> do
      sd <- asks sd
      emit (PUSHLOC (sd - i))
    Global i -> emit (PUSHGLOB i)
