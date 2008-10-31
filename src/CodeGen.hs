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
builtin BAdd = ADD
builtin BMul = MUL
builtin BSub = SUB
builtin BLe  = LEQ
builtin _ = undefined

codeGen :: AST -> [MaMa]
codeGen e = runCg $ do
  codev e
  emit HALT

codev, codeb, codec  :: AST -> Cg ()

-- simple expressions
codeb (Lit x) = emit (LOADC x)
codeb (Builtin bi es) = do
  k <- asks sd
  zipWithM_  (\sd' -> withSd sd' . codeb) [k ..] es
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
  zipWithM_  (\sd' -> withSd sd' . codeb) [sd ..] es
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
    zs = fvs (Abs xs e)
    k = length xs
    g = length zs
    env' = Env.fromList $
             zip xs [Local (-i) | i <- [0..]] ++
             zip zs [Global i   | i <- [0..]]
  zipWithM_ (\sd' -> withSd sd' . getvar) [sd ..] zs
  emits [
    MKVEC g,
    MKFUNVAL a,
    JUMP b,
    LABEL a,
    TARG k]
  withSd 0 $ withEnv env' $ codev e
  emits [RETURN k, LABEL b]
codev (App e es) = do
  a <- newLabel
  emit (MARK a)
  let m = length es
  sd <- asks sd
  zipWithM_ (\sd' -> withSd sd' . codec) [sd + 3 ..] (reverse es)
  withSd (sd + m + 3) (codev e)
  emits [APPLY, LABEL a]
codev (Let False bs e) = do
  sd <- asks sd
  env <- asks env
  let
    step env ((y, e'), i) = do
      withSd (sd + i) $ withEnv env $ codec e'
      return $ Env.insert y (Local (sd + i + 1)) env
  env' <- foldM step env (zip xs [0..])
  withSd (sd + n) $ withEnv env' $ codev e
  emit (SLIDE n)
  where
    n = length bs
    xs = [(y, e') | (y, [], e') <- bs]
codev (Let True bs e) = do
  sd <- asks sd
  env <- asks env
  let
    insert' = uncurry Env.insert
    env' = foldr insert' env $ zip ys (map Local [sd + 1..])
  emit (ALLOC n)
  withSd (sd + n) $ withEnv env' $ do
    forM_ (zip es [n, n-1 ..]) $ \(e', i) -> do
      codec e'
      emit (REWRITE i)
    codev e
  emit (SLIDE n)
  where
    n = length bs
    (ys, es) = unzip [(y, e') | (y, [], e') <- bs]

codec = codev -- undefined

getvar :: Name -> Cg ()
getvar x = do
  env <- asks env
  case Env.lookup env x of
    Local i -> do
      sd <- asks sd
      emit (PUSHLOC (sd - i))
    Global i -> emit (PUSHGLOB i)

-- TODO: find a better place for following code

-- remove many elements from the set, just like (\\)
-- but with list instead of set as second argument
(\\\) :: Ord a => S.Set a -> [a] -> S.Set a
s \\\ [] = s
s \\\ (x : xs) = (S.delete x s) \\\ xs

-- find all free variables of the term
fvs :: AST -> [Name]
fvs = S.toList . fvs'

fvs' (Var x) = S.singleton x
fvs' (Lit _) = S.empty
fvs' (Ifte e t f) = S.unions $ map fvs' [e, t, f]
fvs' (Abs xs e) = fvs' e \\\ xs
fvs' (App e es) = S.unions $ map fvs' (e : es)
fvs' (Let False bs e) = loop bs
  where
    loop [] = fvs' e
    loop ((x, ys, e') : bs) = fvs' e' \\\ ys `S.union` S.delete x (loop bs)
fvs' (Let True bs e) = S.unions (fvs' e : ss) \\\ xs'
  where
    (xs', ss) = unzip [(x, fvs' e' \\\ xs) | (x, xs, e') <- bs]
fvs' (Builtin _ es) = S.unions $ map fvs' es

--

t 1 = Let False [
    ("a", [], Lit 19),
    ("b", [], Builtin BMul [Var "a", Var "a"])]
  (Builtin BAdd [Var "a", Var "b"])

-- factorial of 8
t 2 = Let True [
    ("f", [], Abs ["x", "y"] (
      Ifte (Builtin BLe [Var "y", Lit 1])
        (Var "x")
        (App (Var "f") [
          Builtin BMul [Var "x", Var "y"],
          Builtin BSub [Var "y", Lit 1]])
    ))]
  (App (Var "f") [Lit 1, Lit 8])
