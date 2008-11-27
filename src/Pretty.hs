module Pretty (
  Pretty(..),
  module Text.PrettyPrint.HughesPJ
  ) where

import Text.PrettyPrint.HughesPJ
import qualified Data.ByteString.Char8 as BS

class Pretty a where
  pprint :: a -> Doc

  pprintList :: [a] -> Doc
  pprintList = brackets . hsep . punctuate comma . map pprint

instance Pretty a => Pretty [a] where
  pprint = pprintList

instance Pretty Int where
  pprint = int

instance Pretty Char where
  pprint = char
  pprintList = text

instance Pretty BS.ByteString where
  pprint = text . BS.unpack
