module Eval (Value, EvalError, eval) where

import Control.Arrow (first)

import Ident
import AST
import qualified Data.Map as M
import Control.Monad.Fix

data Value
  = L Literal
  | F (Value -> Eval Value)
  | N
  | C Value Value
  | T [Value]

instance Show Value where
  showsPrec _ (L lit) = shows lit
  showsPrec _ (F _) = showString "<Function>"
  showsPrec _ N = showString "nil"
  showsPrec _ (C v v') = shows v . showString " : " . shows v'
  showsPrec _ (T (v : vs)) = showChar '(' . shows v . loop vs . showChar ')'
    where
      loop [] = id
      loop (v : vs) = showChar ',' . shows v . loop vs

type Env = M.Map Ident Value

eval :: AST Ident -> (String, Either EvalError Value)
eval e = case unEval (eval' e) M.empty of
  (w, x) -> (w [], x)

eval' :: AST Ident -> Eval Value
eval' (Var i) = trace ("var: " ++ show i) >> lookupValue i
eval' (Lit x) = trace ("lit: " ++ show x) >> (return $ L x)
eval' (Ifte e t f) = do
  (L b) <- eval' e
  case b of
    1 -> eval' t
    0 -> eval' f
-- i think the bug is here...
eval' (Abs xs e) = do
  let
    loop [] = eval' e
    loop (y : ys) = do
      trace (show y)
      return $ F $ \v -> updateEnv y v (loop ys)
  trace "abs:"
  loop xs
-- e x y z = ((e x) y) z
eval' (App _ e es) = do
  let
    loop x [] = return x
    loop (F f) (e' : e's) = do
      v <- eval' e'
      x' <- f v
      loop x' e's
  trace "app:"
  x <- eval' e
  trace ":app"
  loop x es
eval' (Let bs e) = do
  let
    loop [] = eval' e
    loop (Single x e' : bs) = do
      v <- eval' e'
      updateEnv x v (loop bs)
    loop (Tuple xs e' : bs) = do
      T vs <- eval' e'
      updateMany (zip xs vs) (loop bs)
  loop bs
eval' (LetRec bs e) = do
  let
    (xs, es) = unzip bs
    loop vs = do
      (T vs) <- updateMany (zip xs vs) $ eval' (MkTuple es)
      return vs
  vs <- mfix loop
  updateMany (zip xs vs) (eval' e)
eval' (MkTuple es) = do
  vs <- mapM eval' es
  return (T vs)
eval' (Select i e) = do
  T vs <- eval' e
  return (vs !! i)
eval' Nil = return N
eval' (Cons e e') = do
  v <- eval' e
  v' <- eval' e'
  return $ C v v'
eval' (Case cbody cnil xh xt ccons) = do
  v <- eval' cbody
  case v of
    N -> eval' cnil
    C vh vt -> do
      updateEnv xh vh (updateEnv xt vt (eval' ccons))
eval' (Builtin bi es) = do
  vs <- mapM eval' es
  evbi bi vs

evbi UNeg [L v]       = return $ L $ (-v)
evbi UNot [L 0]       = return $ L $ 1
evbi UNot [L 1]       = return $ L $ 0
evbi BAdd [L v, L v'] = return $ L $ v + v'
evbi BSub [L v, L v'] = return $ L $ v - v'
evbi BMul [L v, L v'] = return $ L $ v * v'
evbi BDiv [L v, L 0]  = evalThrow DivisionByZero
evbi BDiv [L v, L v'] = return $ L $ v `div` v'
evbi BMod [L v, L 0]  = evalThrow DivisionByZero
evbi BMod [L v, L v'] = return $ L $ v `mod` v'
evbi BEq  [L v, L v'] = return $ L $ fromEnum $ v == v'
evbi BNe  [L v, L v'] = return $ L $ fromEnum $ v /= v'
evbi BLe  [L v, L v'] = return $ L $ fromEnum $ v <= v'
evbi BLt  [L v, L v'] = return $ L $ fromEnum $ v < v'
evbi BGe  [L v, L v'] = return $ L $ fromEnum $ v >= v'
evbi BGt  [L v, L v'] = return $ L $ fromEnum $ v > v'
evbi BOr  [L 0, L 0] = return $ L $ 0
evbi BOr  [L _, L _] = return $ L $ 1
evbi BAnd [L 1, L 1] = return $ L $ 1
evbi BAnd [L _, L _] = return $ L $ 0
evbi _ _ = fail "bad builtin"
--

askEnv :: Eval Env
askEnv = Eval $ \e -> (id, Right e)

updateEnv :: Ident -> Value -> Eval a -> Eval a
updateEnv i v (Eval f) = Eval $ \e -> f (M.insert i v e)

updateMany :: [(Ident, Value)] -> Eval a -> Eval a
updateMany ivs (Eval f) = Eval $ \e -> f (foldr (uncurry M.insert) e ivs)

lookupValue :: Ident -> Eval Value
lookupValue i = do
  env <- askEnv
  case M.lookup i env of
    Nothing -> evalThrow (UnboundIdentifier i)
    Just x -> return x

data EvalError
  = UnboundIdentifier Ident
  | DivisionByZero
  | OtherError String
  deriving Show

newtype Eval a = Eval {
    unEval :: Env -> (String -> String, Either EvalError a)
  }

instance Monad Eval where
  return = returnEval
  (>>=) = bindEval
  (>>) = thenEval
  fail = failEval

instance MonadFix Eval where
  mfix = fixEval

instance Functor Eval where
  fmap = mapEval

trace :: String -> Eval ()
trace str' = Eval $ \_ -> (\str -> str++('\n':str'), Right ())

evalThrow :: EvalError -> Eval a
evalThrow err = Eval $ \_ -> (id, Left err)

mapEval :: (a -> b) -> Eval a -> Eval b
mapEval f (Eval m) = Eval (\e -> case m e of
  (w, Left err) -> (w, Left err)
  (w, Right x) -> (w, Right (f x)))

returnEval :: a -> Eval a
returnEval x = Eval $ \_ -> (id, Right x)

bindEval :: Eval a -> (a -> Eval b) -> Eval b
bindEval (Eval f) m = Eval $ \e -> case f e of
  (w, Left err) -> (w, Left err)
  (w, Right x) -> first (. w) $ unEval (m x) e

failEval :: String -> Eval a
failEval str = Eval $ \_ -> (id, Left (OtherError str))

thenEval :: Eval a -> Eval b -> Eval b
thenEval (Eval f) (Eval g) = Eval $ \e -> case f e of
  (w, Left err) -> (w, Left err)
  (w, Right _) -> first (. w) $ g e

unRight (Right x) = x

fixEval :: (a -> Eval a) -> Eval a
fixEval f = Eval $ \e -> let
    x = unEval (f (unRight (snd x))) e
  in x
