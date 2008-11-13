module CodeGen (codeGen) where

import Control.Monad (forM_, foldM, zipWithM_)

import CgMonad
import AST
import Unique (Supply)
import Ident (Ident)
import MaMa
import qualified Env
import Fvs (fvList)

-- each builtin must correspond to some MaMa instruction
-- well we could generalise this with:
--  builtin :: Builtin -> [MaMa]
builtin :: Builtin -> MaMa
builtin BAdd = ADD
builtin BMul = MUL
builtin BSub = SUB
builtin BLe  = LEQ
builtin BEq  = EQUAL
builtin _ = undefined

codeGen :: Supply -> AST Ident -> [MaMa]
codeGen s e = runCg s $ do
  codev e
  emit HALT

codev, codeb, codec  :: AST Ident -> Cg ()

-- simple expressions
codeb (Lit x) = emit (LOADC x)
codeb (Builtin bi es) = do
  sd <- askSd
  zipWithM_  (\sd' -> withSd sd' . codeb) [sd ..] es
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
  sd <- askSd
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
  sd <- askSd
  let
    zs = fvList (Abs xs e)
    k = length xs
    g = length zs
    env' = Env.fromList $
             zip xs [Env.Local (-i) | i <- [0..]] ++
             zip zs [Env.Global i   | i <- [0..]]
  zipWithM_ (\sd' -> withSd sd' . getvar) [sd ..] zs
  emits [
    MKVEC g,
    MKFUNVAL a,
    JUMP b,
    LABEL a,
    TARG k]
  withSd 0 $ withEnv env' $ codev e
  emits [
    RETURN k,
    LABEL b]
codev (App e es) = do
  a <- newLabel
  sd <- askSd
  let m = length es
  emit (MARK a)
  zipWithM_ (\sd' -> withSd sd' . codec) [sd + 3 ..] (reverse es)
  withSd (sd + m + 3) $ codev e
  emits [
    APPLY,
    LABEL a]
codev (Let NonRec bs e) = do
  sd <- askSd
  env <- askEnv
  let
    n = length bs
    step env ((y, e'), i) = do
      withSd (sd + i) $ withEnv env $ codec e'
      return $ Env.insert y (Env.Local (sd + i + 1)) env
  env' <- foldM step env (zip bs [0..])
  withSd (sd + n) $ withEnv env' $ codev e
  emit (SLIDE n)
codev (Let Rec bs e) = do
  sd <- askSd
  env <- askEnv
  let
    n = length bs
    (ys, es) = unzip bs
    insert' = uncurry Env.insert
    env' = foldr insert' env $ zip ys (map Env.Local [sd + 1..])
  emit (ALLOC n)
  withSd (sd + n) $ withEnv env' $ do
    forM_ (zip es [n, n-1 ..]) $ \(e', i) -> do
      codec e'
      emit (REWRITE i)
    codev e
  emit (SLIDE n)

-- closure stuff
codec e = do
  sd <- askSd
  env <- askEnv
  a <- newLabel
  b <- newLabel
  let
    zs = fvList e
    g = length zs
    env' = Env.fromList $ zip zs (map Env.Global [0..])
  zipWithM_ (\sd' -> withSd sd' . getvar) [sd ..] zs
  emits [
    MKVEC g,
    MKCLOS a,
    JUMP b,
    LABEL a]
  withSd 0 $ withEnv env' $ codev e
  emits [
    UPDATE,
    LABEL b]

getvar :: Ident -> Cg ()
getvar x = do
  env <- askEnv
  case Env.lookup env x of
    Env.Local i -> do
      sd <- askSd
      emit (PUSHLOC (sd - i))
    Env.Global i -> emit (PUSHGLOB i)
