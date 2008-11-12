module Parser(parseFile, parsePuf) where

import qualified PufParser as PP
import qualified PufTokens as PT
import qualified AST
import qualified Data.ByteString.Lazy.Char8 as BS


rawTokens :: FilePath -> IO [PT.Token]
rawTokens = fmap PT.alexScanTokens . BS.readFile

expandIncludes :: [PT.Token] -> IO [PT.Token]
expandIncludes [] = return []
expandIncludes (PT.Include file : xs) = 
    do rt <- rawTokens file 
       ex <- expandIncludes rt 
       rest <- expandIncludes xs
       return $ ex ++ rest
expandIncludes (x : xs) = fmap (x:) $ expandIncludes xs

parseFile :: FilePath -> IO [AST.Binding String]
parseFile fp = fmap PP.parseFile $ rawTokens fp >>= expandIncludes
       
            
parsePuf  :: [PT.Token] -> Either String (AST.AST String)
parsePuf code = Right $ PP.parseExpr code