module Env 
  (Env, Address(..),
  lookup, insert, empty, fromList
  ) where

import Prelude hiding (lookup)
import Data.Map (Map)
import qualified Data.Map as M

import Ident

data Address
  = Local Int
  | Global Int

newtype Env = Env (Map Ident Address)

lookup :: Env -> Ident -> Address
lookup (Env m) id = m M.! id

insert :: Ident -> Address -> (Env -> Env)
insert id addr (Env m) = Env (M.insert id addr m)

empty :: Env
empty = Env M.empty

fromList :: [(Ident, Address)] -> Env
fromList = Env . M.fromList
