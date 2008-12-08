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
import qualified Inline
import qualified AST
import qualified Rename
import qualified Unique
import qualified LastCall
import qualified OptMaMa

data Options = Options {
    printHelp :: Bool,
    mama :: Bool,
    debug :: Bool,
    optMaMa :: Bool,
    optLastCall :: Bool,
    optInline :: Bool,
    includePaths :: [FilePath]
  }
  deriving Show

defaultOpts :: Options
defaultOpts = Options {
    printHelp = False,
    mama = True,
    debug = False,
    optMaMa = False,
    optLastCall = False,
    optInline = False,
    includePaths = ["."]
  }

optsAddInclude :: FilePath -> Options -> Options
optsAddInclude fpath o = o { includePaths = fpath : includePaths o }
optsEnableHelp o = o { printHelp = True }
optsEnableDebug o = o { debug = True }
optsEnableOptMaMa o = o { optMaMa = True }
optsEnableOptLastCall o = o { optLastCall = True }
optsEnableOptInline o = o { optInline = True }
optsDisableMaMa o = o { mama = False }

options :: [OptDescr (Options -> Options)]
options = [
    Option ['h'] ["help"]      (NoArg $ optsEnableHelp)         "Display the help message",
    Option ['I'] ["include"]   (ReqArg optsAddInclude           "DIR") "Include path",
    Option ['g'] ["debug"]     (NoArg $ optsEnableDebug)        "Output some debug info",
    Option ['O'] ["opt-mama"]  (NoArg $ optsEnableOptMaMa)      "Optimise generated mama code",
    Option []    ["last-call"] (NoArg $ optsEnableOptLastCall)  "Enable lastcall optimisation",
    Option []    ["inline"]    (NoArg $ optsEnableOptInline)    "Enable simple inliner",
    Option []    ["no-mama"]   (NoArg $ optsDisableMaMa)        "Do not output generated code"
  ]

handleOpt :: [FilePath] -> Options -> String -> IO ()
handleOpt files opt usage | printHelp opt = putStrLn usage
handleOpt files opt _ = forM_ files $ \file -> do
  when (debug opt) $ do
    putStrLn $ "== " ++ file ++ " =="
  bs <- parseFile (includePaths opt) file
  sup <- Unique.newSupply
  case Rename.rename sup $ AST.Let bs (AST.Var "main") of
    Left err -> putStr $ "Errur: " ++ show err
    Right t -> do
      let t' = (if optLastCall opt then LastCall.detect else id) $
               (if optInline opt   then Inline.inline   else id) $
               depAnal t
      when (debug opt) $ do
        putStrLn "\n== Abstract syntax tree =="
        putStrLn $ render $ pprint t'
        putStrLn "== / ==\n"
      when (mama opt) $ do
        let code = (if optMaMa opt then OptMaMa.optimise else id) $
                   codeGen sup t'
        mapM_ print code

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
    (_, _, xs) -> print xs
