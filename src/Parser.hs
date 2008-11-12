module Parser(parseFile, parsePuf) where

import Control.Monad.Error
import qualified PufParser as PP
import qualified PufTokens as PT
import qualified AST
import qualified Data.ByteString.Lazy.Char8 as BS


rawTokens :: FilePath -> IO [PT.RawToken]
rawTokens = fmap PT.alexScanTokens . BS.readFile

report_fail :: (Monad m) => FilePath -> PT.Pos -> String -> m a
report_fail fp (b,l,c) str = fail (fp ++ ":" ++ show l ++ ":" ++ show c ++ " Lexer:" ++ str)

convertTokens :: FilePath -> [PT.RawToken] -> IO [(PT.Token, PT.Pos, FilePath)]
convertTokens f []                         = return []
convertTokens f (PT.Error     stri p : xs) = report_fail f p stri
convertTokens f (PT.JustToken x    p : xs) = fmap ((x,p,f):) $ convertTokens f xs 
convertTokens f (PT.Include   file p : xs) = 
    do rt   <- rawTokens file 
       ex   <- convertTokens file rt
       rest <- convertTokens f    xs
       return $ ex ++ rest

parseFile :: FilePath -> IO [AST.Binding String]
parseFile fp = do toks <- rawTokens fp >>= convertTokens fp 
--                  print $ map (\(x,_,_) -> x) toks
                  return $ PP.parseFile toks

expr_tokens :: [PT.RawToken] -> Either String [(PT.Token,PT.Pos,FilePath)]
expr_tokens = mapM slfs
  where slfs (PT.Error   str p) = report_fail fn p str
        slfs (PT.Include str p) = report_fail fn p "wat?"
        slfs (PT.JustToken x p) = return (x,p,fn)
        fn = "<interactive>"
            
parsePuf  :: BS.ByteString -> Either String (AST.AST String)
parsePuf code = fmap PP.parseExpr $ expr_tokens $ PT.alexScanTokens code 