module CgMonad (
  Cg,
  emit, emits, newLabel,
  withSd, withEnv, askSd, askEnv,
  runCg
  ) where

import MaMa
import Env as Env

import Unique
import MonadUniq

-- cg = code gen

data CgState = CgState {
    targetCode :: [MaMa] -> [MaMa], -- DList
    supply :: Supply
  }

data CgInfoDown = CgInfoDown {
    env :: Env,
    sd :: Int
  }

newtype Cg a = Cg { unCg :: CgInfoDown -> CgState -> (a, CgState) }

instance Functor Cg where
  fmap = mapCg

instance Monad Cg where
  return = returnCg
  (>>=) = bindCg
  (>>) = thenCg

mapCg :: (a -> b) -> Cg a -> Cg b
mapCg f (Cg fs) = Cg $ \e s -> case fs e s of
  (x, s') -> (f x, s')

returnCg :: a -> Cg a
returnCg x = Cg $ \_ s -> (x, s)

bindCg :: Cg a -> (a -> Cg b) -> Cg b
bindCg (Cg f) k = Cg $ \e s -> case f e s of
  (x, s') -> unCg (k x) e s'

thenCg :: Cg a -> Cg b -> Cg b
thenCg (Cg f) (Cg g) = Cg $ \e s -> case f e s of
  (_, s') -> g e s'

-- emit a mama instruction
emit :: MaMa -> Cg ()
emit i = Cg $ \_ st -> ((), st {
    targetCode = targetCode st . (i:)
  })

-- emit many mama instructions, for convenience
-- emits = mapM_ emit
emits :: [MaMa] -> Cg ()
emits is = Cg $ \_ st -> ((), st {
    targetCode = targetCode st . (is ++)
  })

getInfoDown :: Cg CgInfoDown
getInfoDown = Cg $ \e st -> (e, st)

withInfoDown :: CgInfoDown -> Cg a -> Cg a
withInfoDown e (Cg f) = Cg $ \_ st -> f e st

-- generate a new unique label
newLabel :: Cg Label
newLabel = Cg $ \_ st -> case splitSupply (supply st) of
  (s1, s2) -> (mkLabel $ getUnique s1, st { supply = s2 })

-- run a code gen with given stack difference
withSd :: Int -> Cg a -> Cg a
withSd sd' cg = do
  e <- getInfoDown
  withInfoDown (e { sd = sd' }) cg

askSd :: Cg Int
askSd = Cg $ \e st -> (sd e, st)

-- run a code gen with given environment
withEnv :: Env -> Cg a -> Cg a
withEnv env' cg = do
  e <- getInfoDown
  withInfoDown (e { env = env' }) cg

askEnv :: Cg Env
askEnv = Cg $ \e st -> (env e, st)

runCg :: Supply -> Cg a -> [MaMa]
runCg s (Cg f) = targetCode (snd (f i st)) []
  where
    i = CgInfoDown {
          env = Env.empty,
          sd  = 0
        }

    st = CgState {
          targetCode = id,
          supply = s
        }          
