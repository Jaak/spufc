module Parser(parseFile, parsePuf) where

import Control.Monad.Error
import qualified PufParser as PP
import qualified PufTokens as PT
import qualified AST

import System.IO
import Control.Monad (filterM)
import System.Directory
import System.FilePath

rawTokens :: FilePath -> IO [PT.RawToken]
rawTokens = fmap PT.alexScanTokens . readFile

report_fail :: (Monad m) => FilePath -> PT.Pos -> String -> m a
report_fail fp (b,l,c) str = fail (fp ++ ":" ++ show l ++ ":" ++ show c ++ " Lexer:" ++ str)

convertTokens :: [FilePath] -> FilePath -> [PT.RawToken] -> IO [(PT.Token, PT.Pos, FilePath)]
convertTokens includes = loop
    where loop f []                         = return []
          loop f (PT.Error     stri p : xs) = report_fail f p stri
          loop f (PT.JustToken x    p : xs) = fmap ((x,p,f):) $ loop f xs 
          loop f (PT.Include   file p : xs) = 
              do fs <- filterM doesFileExist $ map (</> file) includes
                 case fs of
                   [] -> fail "Include not found"
                   (fp:_) -> do
                        rt   <- rawTokens fp
                        ex   <- loop fp rt
                        rest <- loop f  xs
                        return $ ex ++ rest

parseFile :: [FilePath] -> FilePath -> IO (AST.Bind String)
parseFile includes fp = fmap PP.parseFile $ rawTokens fp >>= convertTokens includes fp

expr_tokens :: [PT.RawToken] -> Either String [(PT.Token,PT.Pos,FilePath)]
expr_tokens = mapM slfs
  where slfs (PT.Error   str p) = report_fail fn p str
        slfs (PT.Include str p) = report_fail fn p "wat?"
        slfs (PT.JustToken x p) = return (x,p,fn)
        fn = "<interactive>"
            
parsePuf  :: String -> Either String (AST.AST String)
parsePuf code = fmap PP.parseExpr $ expr_tokens $ PT.alexScanTokens code 
