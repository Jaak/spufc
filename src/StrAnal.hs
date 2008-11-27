import Prelude hiding (lookup)
import Ident
import AST
import Type

import Data.Map (Map)
import qualified Data.Map as M

data Demand
  = Strict
  | Lazy

data AbsVal
  = Top
  | Fun Type (AbsVal -> AbsVal) -- we need to have the function type here
  | ApproxFun [Demand] AbsVal
  | Prod [AbsVal]
  | Bot

anyBot :: AbsVal -> Bool
anyBot Bot = True
anyBot Top = False
anyBot (Prod vs) = any anyBot vs
anyBot (Fun _ f) = anyBot (f Top)
anyBot (ApproxFun _ val)    = anyBot val

absApply :: AbsVal -> AbsVal -> AbsVal
absApply Bot _ = Bot
absApply Top _ = Top
absApply (Fun _ f) x = f x
absApply (ApproxFun (d : ds) v) x = case ds of
  [] -> val'
  _  -> ApproxFun ds val'
  where
    val' | evalStrictness d x = Bot
         | otherwise = v

lub :: AbsVal -> AbsVal -> AbsVal
lub Bot v = v
lub v Bot = v
lub (Prod xs) (Prod ys) 
  | length xs == length ys = Prod (zipWith lub xs ys)
  | otherwise = error "type error?"
lub _ _ = Top

glb :: AbsVal -> AbsVal -> AbsVal
glb v v'
  | isFun v || isFun v'
  = if not (anyBot v) && not (anyBot v')
      then Top
      else Bot
  where
    isFun (Fun _ _) = True
    isFun (ApproxFun _ _) = True
    isFun _ = False
glb Top v = v
glb v Top = v
glb (Prod xs) (Prod ys)
  | length xs == length ys = Prod (zipWith glb xs ys)
  | otherwise = error "type error?"
glb _ _ = Bot

evalStrictness :: Demand -> AbsVal -> Bool
evalStrictness Lazy _ = False
evalStrictness Strict Bot = True
evalStrictness _ _ = False

type AbsEnv = Map Ident AbsVal

lookup = M.lookup
insert = M.insert
insertMany = undefined

absEval :: AST Ident -> AbsEnv -> AbsVal
absEval (Var x) env = case lookup x env of
  Nothing -> Top
  Just v -> v
absEval (Lit lit) env = Top
absEval (Ifte e t f) env =  absEval e env `lub` (absEval t env `glb` absEval f env)
absEval (Abs xs e) env = loop xs env
  where
    loop [] env = absEval e env
    loop (x : xs) env = Fun undefined (\v -> loop xs (insert x v env))
absEval (App _ e es) env = loop (absEval e env) (map (\e -> absEval e env) es)
  where
    loop v [] = v
    loop (Fun _ f) (v : vs) = loop (f v) vs
    loop _ _ = error "shit happend in strictness analysis"
absEval (Let bs e) env = absEval e (loop bs env)
  where
    loop [] env = env
    loop (Single x e : es) env = loop es (insert x (absEval e env) env)
    loop (Tuple xs e : es) env = case absEval e env of
      Prod vs | length vs == length xs -> loop es (insertMany (zip xs vs) env)
      _ -> error "oops, type error"
absEval (LetRec bs e) env = absEval e env'
  where
    (binders, rhss) = unzip bs
    strrhss = fix binders rhss env
    env' = insertMany (zip binders strrhss) env
absEval (Builtin _ es) env = foldr lub Bot $ map (\e -> absEval e env) es
absEval (MkTuple es) env = Prod $ map (\e -> absEval e env) es
absEval (Select i e) env = case absEval e env of
  Prod vs | length vs > i -> vs !! i
  _ -> error "oops, type error"
absEval (Nil) env = Top
absEval (Cons _ _) env = Top
absEval (Case cbody cnil _ _ ccons) env =
  absEval cbody env `lub`
  (absEval cnil env `glb` absEval ccons env)

-- TODO
widen :: AbsVal -> AbsVal
widen f@(Fun _ _) = case widenBody of
  ApproxFun ds val -> let
      d = undefined
    in ApproxFun (d : ds) val
  _ -> let
      d = undefined
    in ApproxFun [d] widenBody
  where
    widenBody = widen (absApply f Top)
widen (Prod vs) = Prod (map widen vs)
widen v	= v

-- TODO
fix :: [Ident] -> [AST Ident] -> AbsEnv -> [AbsVal]
fix ids rhss env = map (\rhs -> widen (absEval rhs env')) rhss
  where
    env' = insertMany [(id, Top) | id <- ids] env
