module Rename (rename, RenameError(..)) where

import AST
import Ident
import Unique

import Control.Monad (liftM3, forM, zipWithM, liftM2)
import Data.Map (Map)
import qualified Data.Map as M

data RenameError
  = FreeVariable String
  | OtherRenameError String
  deriving Show

rename :: Supply -> AST String -> Either RenameError (AST Ident)
rename s e = runRn s (rn e)

rn :: AST String -> RnMonad (AST Ident)
rn (Var x) = do
  env <- askEnv
  case M.lookup x env of
    Nothing -> throwRn (FreeVariable x)
    Just id -> return (Var id)
rn (Abs x e) = do
  id <- renameVar x
  e' <- insertEnv x id (rn e)
  return $ Abs id e'
rn (Let (Rec bs) e) = do
  xs <- forM bs $ \(x, e') -> do
    id <- renameVar x 
    return (x, id)
  let
    rn' e = foldr (uncurry insertEnv) (rn e) xs
    step (_, id) (_, e) = do
      e' <- rn' e
      return (id, e')
  bs' <- zipWithM step xs bs
  e' <- rn' e
  return (Let (Rec bs') e')
rn (Let (Single x e) e0) = do
  id <- renameVar x
  e' <- rn e
  e0' <- insertEnv x id (rn e0)
  return $ Let (Single id e') e0'
rn (Let (Tuple xs e) e0) = do
  ids <- mapM renameVar xs
  e' <- rn e
  e0' <- foldr (uncurry insertEnv) (rn e0) (zip xs ids)
  return $ Let (Tuple ids e') e0'
rn (Lit n) = return (Lit n)
rn (Ifte e t f) = liftM3 Ifte (rn e) (rn t) (rn f)
rn (App t e0 e1) = do
  e0' <- rn e0
  e1' <- rn e1
  return $ App t e0' e1'
rn (Builtin bi es) = do
  es' <- mapM rn es
  return $ Builtin bi es'
rn (MkTuple es) = MkTuple `fmap` mapM rn es
rn (Select i e) = Select i `fmap` rn e
rn Nil = return Nil
rn (Cons e e') = liftM2 Cons (rn e) (rn e')
rn (Case cbody cnil xh xt ccons) = do
  cbody' <- rn cbody
  cnil' <- rn cnil
  ih <- renameVar xh
  it <- renameVar xt
  ccons' <- insertEnv xh ih (insertEnv xt it (rn ccons))
  return $ Case cbody' cnil' ih it ccons'
--
-- rename monad
--

type Env = Map String Ident

newtype RnMonad a = Rn {
    unRn :: Env -> Supply -> Either RenameError (a, Supply)
  }

renameVar :: String -> RnMonad Ident
renameVar x = Rn $ \_ st -> case splitSupply st of
  (s1, s2) -> Right (mkIdentFrom x (getUnique s1), s2)

askEnv :: RnMonad Env
askEnv = Rn $ \e st -> Right (e, st)

insertEnv :: String -> Ident -> RnMonad a -> RnMonad a
insertEnv x id (Rn f) = Rn $ \e -> f (M.insert x id e)

runRn :: Supply -> RnMonad a -> Either RenameError a
runRn s (Rn f) = fst `fmap` f M.empty s

throwRn :: RenameError -> RnMonad a
throwRn err = Rn $ \_ _ -> Left err

instance Functor RnMonad where
  fmap = mapRn

instance Monad RnMonad where
  return = returnRn
  (>>=) = bindRn
  (>>) = thenRn
  fail = failRn

failRn :: String -> RnMonad a
failRn err = throwRn (OtherRenameError err)

mapRn f (Rn fs) = Rn $ \e st -> case fs e st of
  Left e -> Left e
  Right (x, st') -> Right (f x, st')

returnRn x = Rn $ \_ s -> Right (x, s)

bindRn (Rn f) k = Rn $ \e st -> case f e st of
  Left e -> Left e
  Right (x, st') -> unRn (k x) e st'

thenRn (Rn f) (Rn g) = Rn $ \e st -> case f e st of
  Left e -> Left e
  Right (_, st') -> g e st'
