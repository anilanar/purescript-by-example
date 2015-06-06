-- Generated by psc-make version 0.6.9.3
module Data.Foreign.Index where
import Data.Function ()
import Data.Foreign.Index ()
import Data.Foreign ()
import Prelude ()
import Prim ()
import Prelude ()
import Data.Either ()
import Data.Foreign ()
import Data.Function ()
infixl 9 !
class Index i where
  (!) :: Data.Foreign.Foreign -> i -> Data.Foreign.F Data.Foreign.Foreign
  hasProperty :: i -> Data.Foreign.Foreign -> Prim.Boolean
  hasOwnProperty :: i -> Data.Foreign.Foreign -> Prim.Boolean
  errorAt :: i -> Data.Foreign.ForeignError -> Data.Foreign.ForeignError
foreign import index :: Prim.Number -> Data.Foreign.Foreign -> Data.Foreign.F Data.Foreign.Foreign
foreign import prop :: Prim.String -> Data.Foreign.Foreign -> Data.Foreign.F Data.Foreign.Foreign
foreign import instance indexString :: Data.Foreign.Index.Index Prim.String
foreign import instance indexNumber :: Data.Foreign.Index.Index Prim.Number
