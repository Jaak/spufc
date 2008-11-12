module Parser(parseFile, parsePuf) where

import qualified PufParser as PP
import qualified PufTokens as PT
import qualified AST
import qualified Data.ByteString.Lazy.Char8 as BS

import Control.Monad (filterM)
import System.Directory
import System.FilePath

rawTokens :: FilePath -> IO [PT.Token]
rawTokens = fmap PT.alexScanTokens . BS.readFile

expandIncludes :: [FilePath] -> [PT.Token] -> IO [PT.Token]
expandIncludes includes = loop
  where
    loop [] = return []
    loop (PT.Include file : xs) =  do
      fs <- filterM doesFileExist $ map (</> file) includes
      case fs of
        [] -> fail "Include not found"
        (f : _) -> do
          rt <- rawTokens f
          ex <- loop rt
          rest <- loop xs
          return $ ex ++ rest
    loop (x : xs) = fmap (x:) $ loop xs

parseFile :: [FilePath] -> FilePath -> IO [AST.Binding String]
parseFile includes fp = fmap PP.parseFile $ rawTokens fp >>= expandIncludes includes

parsePuf  :: [PT.Token] -> Either String (AST.AST String)
parsePuf code = Right $ PP.parseExpr code
