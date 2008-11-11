module Env 
  (Env, Address(..),
  lookup, insert, empty, fromList
  ) where

import AST (Name)

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as M

data Address
  = Local Int
  | Global Int

newtype Env = Env (Map Name Address)

lookup :: Env -> Name -> Address
lookup (Env m) name = m M.! name

insert :: Name -> Address -> (Env -> Env)
insert name addr (Env m) = Env (M.insert name addr m)

empty :: Env
empty = Env M.empty

fromList :: [(Name, Address)] -> Env
fromList = Env . M.fromList
