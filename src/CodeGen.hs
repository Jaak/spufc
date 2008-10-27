module CodeGen (codeGen) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Set as S

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
    zs = fvs (Abs xs e)
    env' = Env.fromList $
             zip xs [Local (-i) | i <- [0..]] ++
             zip zs [Global i   | i <- [0..]]
  zipWithM (\sd' -> withSd sd' . getvar) [sd ..] zs
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

-- TODO: find a better place for this

fvs :: AST -> [Name]
fvs = S.toList . fvs'

fvs' (Var x) = S.singleton x
fvs' (Lit _) = S.empty
fvs' (Ifte e t f) = S.unions (map fvs' [e, t, f])
fvs' (Abs xs e) = foldr S.delete (fvs' e) xs
fvs' (App e es) = S.unions (map fvs' (e : es))
fvs' (Let False bs e) = loop bs
  where
    loop [] = fvs' e
    loop ((x, ys, e') : bs) = foldr S.delete (fvs' e') ys `S.union` S.delete x (loop bs)
fvs' (Let True bs e) = foldr S.delete (S.unions (fvs' e : ss)) xs'
  where
    (xs', ss) = unzip [(x, foldr S.delete (fvs' e') xs) | (x, xs, e') <- bs]
fvs' (Builtin _ es) = S.unions (map fvs' es)
