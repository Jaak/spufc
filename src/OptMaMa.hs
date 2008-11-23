{-# LANGUAGE PatternGuards #-}

module OptMaMa (optimise) where

import MaMa

import Debug.Trace
import Data.List (delete)
import Data.Map (Map)
import qualified Data.Map as M

data Block = Block {
    name :: BlockName,
    basicBlock :: BasicBlock,
    jump :: BlockTail
  }
  deriving Show

-- instructions in reverse order
type BasicBlock = [MaMa]

data BlockName
  = TopBlock
  | NamedBlock Label
  deriving (Eq,Ord,Show)

-- jumps away from block
data BlockTail
  = Halt
  | Jump Label
  | Apply
  | Update
  | Return Int
  deriving (Show)

instance Eq Block where
  b == b' = (name b) == (name b')

instance Ord Block where
  compare b b' = compare (name b) (name b')

blockify :: [MaMa] -> [Block]
blockify = loop TopBlock []
  where
    loop header bb (x : xs) = case x of
      JUMP to  -> mkBlock (Jump to) : rest
      HALT     -> mkBlock Halt : rest
      APPLY    -> mkBlock Apply : rest
      UPDATE   -> mkBlock Update : rest
      RETURN k -> mkBlock (Return k) : rest
      _        -> loop header (x : bb) xs
      where
        rest = dropdead xs
        mkBlock typ = Block header bb typ
    loop _ _ _ = error "Internal error (blockify)"

    dropdead [] = []
    dropdead (LABEL lbl : xs) = loop (NamedBlock lbl) [] xs
    dropdead (_ : xs) = trace "Found dead code" (dropdead xs)

unblockify :: Block -> [MaMa]
unblockify block = (unName . loop (basicBlock block) . unTail) []
  where
    unName = case name block of
      TopBlock -> id
      NamedBlock l -> (LABEL l :)

    loop [] = id
    loop (x : xs) = loop xs . (x :)

    unTail = case jump block of
      Halt       -> (HALT :)
      (Jump l)   -> (JUMP l :)
      Apply      -> (APPLY :)
      Update     -> (UPDATE :)
      (Return k) -> (RETURN k :)

-- labels that given block mentions
refs :: Block -> [Label]
refs block = case jump block of
  Jump x -> x : xs
  _      -> xs
  where
    xs = loop (basicBlock block)

    loop [] = []
    loop (x : xs) = case x of
      JUMP l     -> error "Impossible has happend (refs)"
      LABEL l    -> l : rest --
      MKCLOS l   -> l : rest
      MKFUNVAL l -> l : rest
      MARK l     -> l : rest
      JUMPZ l    -> l : rest
      TLIST l    -> l : rest
      _          -> rest
      where
        rest = loop xs

type DepGr = Map Label [BlockName]

depGr :: [Block] -> DepGr
depGr [] = M.empty
depGr (x : xs) = foldr insert (depGr xs) [(ref, name x) | ref <- refs x]
  where
    insert (from, to) = M.insertWith (++) from [to]

buildMap :: [Block] -> Map BlockName Block
buildMap [] = M.empty
buildMap (x : xs) = M.insert (name x) x (buildMap xs)

mergeBlocks :: Block -> Block -> Block
mergeBlocks b b' = trace msg $ Block {
    name = name b,
    basicBlock = basicBlock b' ++ basicBlock b,
    jump = jump b'
  }
  where
    msg = "merge: " ++ show (name b) ++ ", " ++ show (name b') 

reorderBlocks :: [Block] -> [Block]
reorderBlocks blocks = trace msg $ loop (buildMap blocks) (map name blocks)
  where
    gr :: DepGr
    gr = depGr blocks

    msg = unlines (map show blocks)

    loop _ [] = []
    loop m (name : xs) = case M.lookup name m of
      Nothing -> error "internal error (reorder blocks)"
      Just block
        | Jump lbl <- jump block,
          let name' = NamedBlock lbl,
          Just [_] <- M.lookup lbl gr, -- we only have single reference to current block
          Just block' <- M.lookup name' m
            -> loop (M.insert name (mergeBlocks block block') m) (name : delete name' xs)
        | otherwise -> block : loop m xs


optimise :: [MaMa] -> [MaMa]
optimise = concatMap unblockify . reorderBlocks . blockify
