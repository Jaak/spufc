module Fvs (fvs, fvList) where

import qualified Data.Set as S
import Data.Set (Set)

import AST

fvList :: Ord a => AST a -> [a]
fvList = S.toList . fvs

(\\\) :: Ord a => S.Set a -> [a] -> S.Set a
s \\\ [] = s
s \\\ (x : xs) = (S.delete x s) \\\ xs

fvs :: Ord a => AST a -> Set a
fvs (Var x) = S.singleton x
fvs (Lit _) = S.empty
fvs (Ifte e t f) = S.unions $ map fvs [e, t, f]
fvs (Abs xs e) = fvs e \\\ xs
fvs (App e es) = S.unions $ map fvs (e : es)
fvs (Let NonRec bs e) = loop bs
  where
    loop [] = fvs e
    loop ((x, e') : bs) = fvs e' `S.union` S.delete x (loop bs)
fvs (Let Rec bs e) = S.unions (fvs e : ss) \\\ xs'
  where
    (xs', ss) = unzip [(x, fvs e') | (x, e') <- bs]
fvs (Builtin _ es) = S.unions $ map fvs es
