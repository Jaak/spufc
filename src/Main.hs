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
    eval :: Bool,
    mama :: Bool,
    debug :: Bool,
    optMaMa :: Bool,
    includePaths :: [FilePath]
  }
  deriving Show

defaultOpts :: Options
defaultOpts = Options {
    printHelp = False,
    eval = False,
    mama = True,
    debug = False,
    optMaMa = False,
    includePaths = ["."]
  }

optsAddInclude :: FilePath -> Options -> Options
optsAddInclude fpath o = o { includePaths = fpath : includePaths o }

optsEnableHelp o = o { printHelp = True }

optsEnableEval o = o { eval = True, mama = False }

optsEnableMama o = o { mama = True, eval = False }

optsEnableDebug o = o { debug = True }

optsEnableOptMaMa o = o { optMaMa = True }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['h'] ["help"] (NoArg $ optsEnableHelp) "Display the help message",
    Option ['I'] ["include"] (ReqArg optsAddInclude "DIR") "Include paths",
    Option ['e'] ["evaluate"] (NoArg $ optsEnableEval) "Evaluate given file",
    Option ['g'] ["debug"] (NoArg $ optsEnableDebug) "Output some debug info",
    Option ['O'] ["opt-mama"] (NoArg $ optsEnableOptMaMa) "Optimise generated mama code",
    Option [] ["mama"] (NoArg $ optsEnableMama) "Output MaMa bytecode"
  ]

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case getOpt Permute options args of
    (opts, files, []) -> do
      let opt = foldr ($) defaultOpts opts
      if printHelp opt
        then putStr $ usageInfo "Help! I'm trapped in the IO monad!" options
        else forM_ files $ \file -> do
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
              when (mama opt) $ do
                let f = if optMaMa opt then OptMaMa.optimise else id
                mapM_ print $ f $ codeGen sup t'
              when (eval opt) $ return ()
