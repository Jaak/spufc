module CgMonad (
  Cg, CgState(..), CgInfoDown(..),
  emit, emits, newLabel, withSd, withEnv,
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

-- emit many mama instructions, for convenience
-- emits = mapM_ emit
emits :: [MaMa] -> Cg ()
emits is = modify $ \st -> st {
    targetCode = targetCode st . (is ++)
  }

-- generate a new unique label
newLabel :: Cg Label
newLabel = do
  (l : ls) <- gets labelSupply
  modify $ \st -> st { labelSupply = ls }
  return l

-- run a code gen with given stack difference
withSd :: Int -> Cg a -> Cg a
withSd sd' = local (\st -> st { sd = sd' })

-- run a code gen with given environment
withEnv :: Env -> Cg a -> Cg a
withEnv env' = local (\st -> st { env = env' })

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
