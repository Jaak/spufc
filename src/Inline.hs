module Inline (inline) where

import Ident
import AST

import Debug.Trace
import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as M

--
--

newtype OccurTable = OccurTable {
    unOC :: Map Ident Int
  }

lookup :: OccurTable -> Ident -> Int
lookup (OccurTable tbl) i = case M.lookup i tbl of
  Just n -> n
  Nothing -> 0

singleton x = OccurTable (M.singleton x 1)
empty = OccurTable M.empty
unions = OccurTable . M.unionsWith (+) . map unOC

--
--

occurs :: AST Ident -> OccurTable
occurs (Var x) = singleton x
occurs (Lit _) = empty
occurs (Ifte e t f) = unions [occurs e, occurs t, occurs f]
occurs (Abs _ e) = occurs e
occurs (App _ e e') = unions [occurs e, occurs e']
occurs (Let (Rec bs) e) = unions (map occurs (e : map snd bs))
occurs (Let (Single x e) e0) = unions [occurs e, occurs e0]
occurs (Let (Tuple xs e) e0) = unions [occurs e, occurs e0]
occurs (Builtin _ es) = unions (map occurs es)
occurs (MkTuple es) = unions (map occurs es)
occurs (Select _ e) = occurs e
occurs Nil = empty
occurs (Cons e e') = unions [occurs e, occurs e']
occurs (Case e1 e2 _ _ e3) = unions [occurs e1, occurs e2, occurs e3]

replace :: Ident -> AST Ident -> AST Ident -> AST Ident
replace i e' = loop
  where
    loop (Var x) | x == i = e'
    loop (Var x) = Var x
    loop (Lit x) = Lit x
    loop (Ifte e t f) = Ifte (loop e) (loop t) (loop f)
    loop (Abs xs e) = Abs xs (loop e)
    loop (App t e e') = App t (loop e) (loop e')
    loop (Let (Single x e) e0) = Let (Single x (loop e)) (loop e0)
    loop (Let (Tuple xs e) e0) = Let (Tuple xs (loop e)) (loop e0)
    loop (Let (Rec bs) e) = Let (Rec [(x, loop e) | (x, e) <- bs]) (loop e)
    loop (Builtin bi es) = Builtin bi (map loop es)
    loop (MkTuple es) = MkTuple (map loop es)
    loop (Select i e) = Select i (loop e)
    loop Nil = Nil
    loop (Cons e e') = Cons (loop e) (loop e')
    loop (Case cbody cnil xh xt ccons) =
      Case (loop cbody) (loop cnil) xh xt (loop ccons)

inline :: AST Ident -> AST Ident
inline ast = loop ast
  where
    tbl = occurs ast

    loop (Var x) = Var x
    loop (Lit x) = Lit x
    loop (Ifte e t f) = Ifte (loop e) (loop t) (loop f)
    loop (Abs xs e) = Abs xs (loop e)
    loop (App t e e') = App t (loop e) (loop e')
    loop (Let (Single x e) e0)
      | tbl `lookup` x == 1 = loop (replace x e e0)
      | otherwise = Let (Single x e) (loop e0)
    loop (Let (Tuple xs e) e0) = Let (Tuple xs e) (loop e0)
    loop (Let (Rec bs) e) = Let (Rec [(x, loop e) | (x, e) <- bs]) (loop e)
    loop (Builtin bi es) = Builtin bi (map loop es)
    loop (MkTuple es) = MkTuple (map loop es)
    loop (Select i e) = Select i (loop e)
    loop Nil = Nil
    loop (Cons e e') = Cons (loop e) (loop e')
    loop (Case cbody cnil xh xt ccons) =
      Case (loop cbody) (loop cnil) xh xt (loop ccons)
