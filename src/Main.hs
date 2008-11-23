module Main (main) where

import Parser
import Pretty
import System.Environment
import System.Console.GetOpt
import System.IO
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Control.Monad (forM_, when)

import CodeGen
import Parser
import Pretty
import DepAnal
import qualified AST
import qualified Rename
import qualified Unique
import qualified LastCall
import qualified OptMaMa

data Options = Options {
    printHelp :: Bool,
    debug :: Bool,
    optMaMa :: Bool,
    includePaths :: [FilePath]
  }
  deriving Show

defaultOpts :: Options
defaultOpts = Options {
    printHelp = False,
    debug = False,
    optMaMa = False,
    includePaths = ["."]
  }

optsAddInclude :: FilePath -> Options -> Options
optsAddInclude fpath o = o { includePaths = fpath : includePaths o }
optsEnableHelp o = o { printHelp = True }
optsEnableDebug o = o { debug = True }
optsEnableOptMaMa o = o { optMaMa = True }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['h'] ["help"]      (NoArg $ optsEnableHelp)      "Display the help message",
    Option ['I'] ["include"]   (ReqArg optsAddInclude        "DIR") "Include path",
    Option ['g'] ["debug"]     (NoArg $ optsEnableDebug)     "Output some debug info",
    Option ['O'] ["opt-mama"]  (NoArg $ optsEnableOptMaMa)   "Optimise generated mama code"
  ]

handleOpt :: [FilePath] -> Options -> String -> IO ()
handleOpt files opt usage | printHelp opt = putStrLn usage
handleOpt files opt _ = forM_ files $ \file -> do
  when (debug opt) $ do
    putStrLn $ "== " ++ file ++ " =="
  bs <- parseFile (includePaths opt) file
  sup <- Unique.newSupply
  case Rename.rename sup $ AST.LetRec bs (AST.Var "main") of
    Left err -> putStr $ "Errur: " ++ show err
    Right t -> do
      let t' = LastCall.detect (depAnal t)
      when (debug opt) $ do
        putStrLn "\n== Abstract syntax tree =="
        putStrLn $ prettyAST $ t'
        putStrLn "== / ==\n"
      let f = if optMaMa opt then OptMaMa.optimise else id
      mapM_ print $ f $ codeGen sup t'

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case getOpt Permute options args of
    (opts, files, []) -> do
      let opt = foldr ($) defaultOpts opts
      handleOpt
        files
        opt
        (usageInfo "./tm [OPTION] [FILE]" options)
