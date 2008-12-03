module Fvs (fvs, fvList) where

import qualified Data.Set as S
import Data.Set (Set)

import AST

fvList :: Ord a => AST a -> [a]
fvList = S.toList . fvs

(\\\) :: Ord a => S.Set a -> [a] -> S.Set a
s \\\ [] = s
s \\\ (x : xs) = S.delete x s \\\ xs

fvs :: Ord a => AST a -> Set a
fvs (Var x) = S.singleton x
fvs (Lit _) = S.empty
fvs (Ifte e t f) = S.unions $ map fvs [e, t, f]
fvs (Abs x e) = S.delete x (fvs e)
fvs (App _ e e') = fvs e `S.union` fvs e'
fvs (Let (Single x e) e0) = fvs e `S.union` S.delete x (fvs e0)
fvs (Let (Tuple xs e) e0) = fvs e `S.union` (fvs e0 \\\ xs)
fvs (Let (Rec bs) e) = S.unions (fvs e : ss) \\\ xs'
  where
    (xs', ss) = unzip [(x, fvs e') | (x, e') <- bs]
fvs (Builtin _ es) = S.unions $ map fvs es
fvs (MkTuple es) = S.unions $ map fvs es
fvs (Select _ e) = fvs e
fvs Nil = S.empty
fvs (Cons e e') = S.union (fvs e) (fvs e')
fvs (Case body cnil xh xt ccons) = S.unions [
  fvs body,
  fvs cnil,
  fvs ccons \\\ [xh, xt]]
