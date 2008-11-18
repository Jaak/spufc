module Main (main) where

import Parser
import Pretty
import System.Environment
import System.Console.GetOpt
import System.IO
import qualified Data.ByteString.Lazy.Char8 as ByteString
import Control.Monad (forM_)

import Parser
import Pretty
import DepAnal
import qualified AST
import qualified Rename
import qualified Unique

data Options = Options {
    printHelp :: Bool,
    includePaths :: [FilePath]
  }
  deriving Show

defaultOpts :: Options
defaultOpts = Options {
    printHelp = False,
    includePaths = ["."]
  }

optsAddInclude :: FilePath -> Options -> Options
optsAddInclude fpath o = o { includePaths = fpath : includePaths o }

optsEnableHelp o = o { printHelp = True }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['h'] ["help"] (NoArg $ optsEnableHelp) "Display the help message",
    Option ['I'] ["include"] (ReqArg optsAddInclude "DIR") "Include paths"
  ]

main :: IO ()
--main = (fmap head $ getArgs) >>= parseFile >>= print 
main = do
  hSetBuffering stdout NoBuffering
  args <- getArgs
  case getOpt Permute options args of
    (opts, files, []) -> do
      let opt = foldr ($) defaultOpts opts
      if printHelp opt
        then putStr $ usageInfo "Help! I'm trapped in the IO monad!" options
        else forM_ files $ \file -> do
          putStrLn $ "== " ++ file ++ " =="
          bs <- parseFile (includePaths opt) file
          sup <- Unique.newSupply
          case Rename.rename sup $ AST.LetRec bs (AST.Var "main") of
            Left _ -> putStrLn "Errur"
            Right t -> putStrLn $ prettyAST $ depAnal t
