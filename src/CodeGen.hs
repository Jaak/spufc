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
builtin :: Builtin -> Cg ()
builtin UNeg = emit NEG
builtin UNot = emit NOT
builtin BAdd = emit ADD
builtin BSub = emit SUB
builtin BMul = emit MUL
builtin BDiv = emit DIV
builtin BMod = emit MOD
builtin BEq  = emit EQUAL
builtin BNe  = emit NEQ
builtin BLe  = emit LEQ
builtin BLt  = emit LE
builtin BGe  = emit GEQ
builtin BGt  = emit GR
builtin BOr  = emit OR
builtin BAnd = emit AND
builtin _    = fail "oops, we hit a builtin that is not known!"

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
  builtin bi
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
  builtin bi
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
codev (Let bs e) = do
  let
    loop j [] = do
      codev e
      emit (SLIDE (j-1))
    loop j (b : bs) = do
      sd <- askSd
      env <- askEnv
      case b of
        Single x e' -> do
          codec e'
          withSd (sd + 1) $ withEnv (Env.insert x (Env.Local (sd + 1)) env) $ loop (j + 1) bs
        Tuple xs e' -> do
          let k = length xs
              insert' = uncurry Env.insert
          codev e'
          emit (GETVEC k)
          withSd (sd + k) $ withEnv (foldr insert' env (zip xs (map Env.Local [sd + 1..]))) $ loop (j + k) bs
  loop 1 bs
--    loop [] = codev e
--    loop (Single x e' : bs) = do
--      sd <- askSd
--      env <- askEnv
--      codec e'
--      withSd (sd + 1) $ withEnv (Env.insert x (Env.Local (sd + 1)) env) $ loop bs
--      emit (SLIDE 1)
--    loop (Tuple xs e' : bs) = do
--      sd <- askSd
--      env <- askEnv
--      let k = length xs
--          insert' = uncurry Env.insert
--      codev e'
--      emit (GETVEC k)
--      withSd (sd + k) $ withEnv (foldr insert' env (zip xs (map Env.Local [sd + 1..]))) $ loop bs
--      emit (SLIDE k)
--  loop bs

codev (LetRec bs e) = do
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
codev (MkTuple es) = do
  emit (COMMENT "<tuple>")
  let k = length es
  sd <- askSd
  zipWithM_ (\i -> withSd (sd + i) . codec) [0..] es
  emit (MKVEC k)
  emit (COMMENT "</tuple>")
codev (Select j e) = do
  codev e
  emit (GET j)
  emit EVAL
codev Nil = emit NIL
codev (Cons e e') = do
  sd <- askSd
  codec e
  withSd (sd + 1) $ codec e'
  emit CONS
codev (Case cbody cnil xh xt ccons) = do
  a <- newLabel
  b <- newLabel
  sd <- askSd
  env <- askEnv
  codev cbody
  emit (TLIST a)
  codev cnil
  emit (JUMP b)
  emit (LABEL a)
  let
    env' = Env.insert xh (Env.Local (sd + 1)) $
           Env.insert xt (Env.Local (sd + 2)) $ env
  withSd (sd + 2) $ withEnv env' $ codev ccons
  emit (SLIDE 2)
  emit (LABEL b)

-- closure stuff
codec e@(Abs _ _) = codev e

codec e@(Lit _) = codev e

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
