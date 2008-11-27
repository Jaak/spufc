module Unique 
  (Unique, Supply,
   getUnique, getUniques, splitSupply,
   newSupply)
  where

import Pretty

import Control.Monad(liftM)
import Data.IORef
import System.IO.Unsafe

newtype Unique = U Int

instance Eq Unique where
  (U n) == (U m) = n == m

instance Ord Unique where
  (U n) `compare` (U m) = n `compare` m

instance Show Unique where
  showsPrec n (U u) = showChar 'u' . showsPrec n u

instance Pretty Unique where
  pprint (U u) = char 'u' <> pprint u

data Supply = Supply Unique Supply Supply

getUnique :: Supply -> Unique
getUnique (Supply u _ _) = u

getUniques :: Supply -> [Unique]
getUniques (Supply n _ s') = n : getUniques s'

splitSupply :: Supply -> (Supply, Supply)
splitSupply (Supply _ s s') = (s, s')

newtype IOSupply = IOSupply (IORef Unique)

newIOSupply :: IO IOSupply
newIOSupply = liftM IOSupply $ newIORef (U 0)

getUniqueIO :: IOSupply -> IO Unique
getUniqueIO (IOSupply s) = do
    U u <- readIORef s
    writeIORef s $ U $ u+1
    return $ U u

{-# NOINLINE getSupply #-}
getSupply :: IOSupply -> IO Supply
getSupply s = do
    s1 <- unsafeInterleaveIO $ getSupply s
    s2 <- unsafeInterleaveIO $ getSupply s
    n  <- unsafeInterleaveIO $ getUniqueIO s
    return $ Supply n s1 s2

{-# NOINLINE newSupply #-}
newSupply :: IO Supply
newSupply = newIOSupply >>= getSupply
