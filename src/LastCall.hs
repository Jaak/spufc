module LastCall (detect) where

import AST

data Ctx
  = NotLast
  | IsLast Int

detect :: AST a -> AST a
detect = loop NotLast

loop _ (Var x) = Var x
loop _ (Lit x) = Lit x
loop ctx (Ifte e et ef) = Ifte (loop NotLast e) (loop ctx et) (loop ctx ef)
loop (IsLast k) (Abs x e) = Abs x (loop (IsLast (k + 1)) e)
loop NotLast (Abs x e) = Abs x (loop (IsLast 1) e)
loop NotLast (App _ e e') = App RegularCall (loop NotLast e) (loop NotLast e')
loop (IsLast k) (App _ e@(App _ _ _) e') = App RegularCall (loop (IsLast k) e) (loop NotLast e')
loop (IsLast k) (App _ e e') = App (LastCall k) (loop NotLast e) (loop NotLast e')
loop ctx (Let (Rec bs) e) = let
    f (x, e') = (x, loop NotLast e')
  in Let (Rec (map f bs)) (loop NotLast e)
loop _ (Let (Single x e') e) = Let (Single x (loop NotLast e')) (loop NotLast e)
loop _ (Let (Tuple xs e') e) = Let (Tuple xs (loop NotLast e')) (loop NotLast e)
loop _ (Builtin bi es) = Builtin bi (map (loop NotLast) es)
loop _ (MkTuple es) = MkTuple (map (loop NotLast) es)
loop _ (Select i e) = Select i (loop NotLast e)
loop _ Nil = Nil
loop _ (Cons e e') = Cons (loop NotLast e) (loop NotLast e')
loop ctx (Case cbody cnil xh xt ccons) =
  Case (loop NotLast cbody) (loop ctx cnil) xh xt (loop ctx ccons)
