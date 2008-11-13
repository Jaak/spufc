module DepAnal (depAnal, DepAnalError) where

import Debug.Trace
import Data.Maybe (fromJust)
import Data.List ((\\))
import Control.Arrow (second)
import Data.Graph.Inductive
import Data.Set (Set)
import qualified Data.Set as S

import Ident
import AST
import Fvs (fvs)

type DepAnalError = String

data DANode
  = Root (AST Ident) (Set Ident)
  | Node Ident (AST Ident) (Set Ident)
  deriving Show

depOn :: DANode -> DANode -> Bool
depOn (Root _ _) _ = False
depOn (Node x _ _) n = x `S.member` nodeFvs n

nodeFvs (Root _ s) = s
nodeFvs (Node _ _ s) = s

depAnal :: AST Ident -> AST Ident
depAnal (Var x) = Var x
depAnal (Lit n) = Lit n
depAnal (Abs xs e) = Abs xs (depAnal e)
depAnal (App e es) = App (depAnal e) (map depAnal es)
depAnal (Let Rec bs e) = toAST . topsort' . toStrong . rmUnreachable $ mkDepGraph bs e
depAnal (Let NonRec bs e) = Let NonRec (map (second depAnal) bs) (depAnal e)
depAnal (Ifte e t f) = Ifte (depAnal e) (depAnal t) (depAnal f)
depAnal (Builtin bi es) = Builtin bi (map depAnal es)

-- edges are backwards because we want to remove nodes that are
-- not reachable from |e|.
mkDepGraph :: [Binding Ident] -> AST Ident -> Gr DANode ()
mkDepGraph bs e = graph
  where
    univ = S.fromList $ map fst bs
    fvs' e = fvs e `S.intersection` univ
    mkNode (x, e') = Node x e' (fvs' e')
    nodes = (0, Root e (fvs' e)) : zip [1..] (map mkNode bs)
    edges = [(j, i, ()) | (i, n) <- nodes, (j, m) <- nodes, n `depOn` m]
    graph = mkGraph nodes edges

-- also flip the edges to correct order
toStrong :: Gr DANode () -> Gr [DANode] ()
toStrong g = graph
  where
    nodes = zip [0..] . map (map (fromJust . lab g)) $ scc g
    p ns ms = or [n `depOn` m | n <- ns, m <- ms]
    edges = [(i, j, ()) | (i, ns) <- nodes, (j, ms) <- nodes, ns `p` ms]
    graph = mkGraph nodes edges

rmUnreachable :: Gr DANode () -> Gr DANode ()
rmUnreachable g = delNodes (ns \\ rs) g
  where
    rs = reachable 0 g
    ns = nodes g

toAST :: [[DANode]] -> AST Ident
toAST [[Root e _]] = e
toAST nss = case collectNonrec nss of
  ([], ns : nss) -> Let Rec (map unNode ns) (toAST nss)
  (ns, nss) -> Let NonRec (map unNode ns) (toAST nss)

unNode :: DANode -> Binding Ident
unNode (Node x e _) = (x, e)

collectNonrec :: [[DANode]] -> ([DANode], [[DANode]])
collectNonrec ([n@(Node x _ s)] : nss)
  | x `S.member` s = ([], [n]:nss)
  | otherwise = let
      (ns, nss') = collectNonrec nss
    in (n : ns, nss')
collectNonrec nss = ([], nss)
