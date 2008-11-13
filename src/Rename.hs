module Rename (rename, RenameError) where

import AST
import Ident
import Unique

import Control.Monad (liftM3, forM, zipWithM)
import Data.Map (Map)
import qualified Data.Map as M

type RenameError
  = String

rename :: Supply -> AST String -> Either RenameError (AST Ident)
rename s e = runRn s (rn e)

rn :: AST String -> RnMonad (AST Ident)
rn (Var x) = do
  env <- askEnv
  case M.lookup x env of
    Nothing -> fail $ "Free variable " ++ show x
    Just id -> return (Var id)
rn (Abs xs e) = do
  (xs', e') <- loop xs
  return $ Abs xs' e'
  where
    loop [] = do
      e' <- rn e
      return ([], e')
    loop (x : xs) = do
      id <- renameVar x
      (ids, e) <- insertEnv x id (loop xs)
      return (id : ids, e)
rn (Let NonRec bs e) = do
  (bs', e') <- loop bs
  return (Let NonRec bs' e')
  where
    loop [] = do
      e' <- rn e
      return ([], e')
    loop ((x, e) : bs) = do
      id <- renameVar x 
      ne <- rn e
      (nes, e) <- insertEnv x id (loop bs)
      return ((id, ne) : nes, e)
rn (Let Rec bs e) = do
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
  return (Let Rec bs' e')
rn (Lit n) = return (Lit n)
rn (Ifte e t f) = liftM3 Ifte (rn e) (rn t) (rn f)
rn (App e es) = do
  e' <- rn e
  e's <- mapM rn es
  return $ App e' e's
rn (Builtin bi es) = do
  es' <- mapM rn es
  return $ Builtin bi es'

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

instance Functor RnMonad where
  fmap = mapRn

instance Monad RnMonad where
  return = returnRn
  (>>=) = bindRn
  (>>) = thenRn
  fail = failRn

failRn :: String -> RnMonad a
failRn err = Rn $ \_ _ -> Left err

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
