module Main (main) where

import qualified PufParser
import qualified PufTokens
import qualified AST
import qualified Data.ByteString.Lazy.Char8 as ByteString

main :: IO ()
main = do
  s <- ByteString.takeWhile (/= '\n') `fmap` ByteString.getContents
  print $ PufParser.parse $ PufTokens.alexScanTokens s
