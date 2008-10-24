module CodeGen (
  Cg, CgState(..), CgInfoDown(..),
  emit, newLabel,
  runCg
  ) where

import MaMa
import Env

import Control.Monad.Reader
import Control.Monad.State

-- cg = code gen

data CgState = CgState {
    targetCode :: [MaMa] -> [MaMa],
    labelSupply :: [Label]
}

data CgInfoDown = CgInfoDown {
    env :: Env,
    sd :: Int
  }

type Cg = ReaderT CgInfoDown (State CgState)

emit :: MaMa -> Cg ()
emit i = modify $ \st -> st {
    targetCode = targetCode st . (i:)
  }

newLabel :: Cg Label
newLabel = do
  (l : ls) <- gets labelSupply
  modify $ \st -> st { labelSupply = ls }
  return l

runCg :: CgInfoDown -> CgState -> Cg a -> [MaMa]
runCg i st cg = targetCode (execState (runReaderT cg i) st) []
