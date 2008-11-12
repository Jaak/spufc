module Main (main) where

import Parser
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as ByteString

main :: IO ()
main = (fmap head $ getArgs) >>= parseFile >>= print
     
