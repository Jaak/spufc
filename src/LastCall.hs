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
loop _ (Abs xs e) = Abs xs (loop (IsLast (length xs)) e)
loop NotLast (App _ e es) = App RegularCall (loop NotLast e) (map (loop NotLast) es)
loop (IsLast k) (App _ e es) = App (LastCall k) (loop NotLast e) (map (loop NotLast) es)
loop ctx (Let bs e) = let
    f (Single x e') = Single x (loop NotLast e')
    f (Tuple xs e') = Tuple xs (loop NotLast e')
  in Let (map f bs) (loop NotLast e) -- XXX: i'm not sure why we can't keep context
loop ctx (LetRec bs e) = let
    f (x, e') = (x, loop NotLast e')
  in LetRec (map f bs) (loop NotLast e)  -- XXX: i'm not sure why we can't keep context
loop _ (Builtin bi es) = Builtin bi (map (loop NotLast) es)
loop _ (MkTuple es) = MkTuple (map (loop NotLast) es)
loop _ (Select i e) = Select i (loop NotLast e)
loop _ Nil = Nil
loop _ (Cons e e') = Cons (loop NotLast e) (loop NotLast e')
loop ctx (Case cbody cnil xh xt ccons) =
  Case (loop NotLast cbody) (loop ctx cnil) xh xt (loop ctx ccons)
