module CodeGen (
  Cg, CgState(..), CgInfoDown(..),
  emit, newLabel,
  runCg
  ) where

import MaMa
import Env as Env

import Control.Monad.Reader
import Control.Monad.State

-- cg = code gen

data CgState = CgState {
    targetCode :: [MaMa] -> [MaMa], -- DList
    labelSupply :: [Label]
}

data CgInfoDown = CgInfoDown {
    env :: Env,
    sd :: Int
  }

type Cg = ReaderT CgInfoDown (State CgState)

-- emit a mama instruction
emit :: MaMa -> Cg ()
emit i = modify $ \st -> st {
    targetCode = targetCode st . (i:)
  }

-- generate a new unique label
newLabel :: Cg Label
newLabel = do
  (l : ls) <- gets labelSupply
  modify $ \st -> st { labelSupply = ls }
  return l

runCg :: Cg a -> [MaMa]
runCg cg = targetCode (execState (runReaderT cg i) st) []
  where
    i = CgInfoDown {
          env = Env.empty,
          sd  = 0
        }

    st = CgState {
          targetCode = id,
          labelSupply = allLabels
        }          
