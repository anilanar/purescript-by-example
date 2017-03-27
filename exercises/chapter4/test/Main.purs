module Test.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (filter, head, length)
import Data.Maybe (Maybe(..))
import Data.Path (Path, filename, root)
import Data.Tuple (Tuple, fst, snd)
import FileOperations (whereIs)
import FileOperations.Internal (asTuples)
import Prelude (Unit, bind, eq, map, ($), (<<<), (==))
import Test.Assert (ASSERT, assert)

main :: forall t1. Eff (assert :: ASSERT, console :: CONSOLE | t1) Unit
main = do
    testAsTuples
    assert (whereIs "/" == Nothing)
    assert (map filename (whereIs "/bin/") == Just "/")

testAsTuples :: forall t1. Eff (assert :: ASSERT, console :: CONSOLE | t1) Unit
testAsTuples = do
    let tuples = asTuples root

    log "/bin/ should occur once"
    assert $ countTuple "/bin/" tuples == 1

    log "/bin/ls should occur once"
    assert $ countTuple "/bin/ls" tuples == 1

    log "/ should have Nothing as its parent"
    assert $ findTuple "/" tuples == Nothing

    log "/bin/ should have / as its parent"
    assert $ (map parentName $ findTuple "/bin/" tuples) == Just "/"

    log "/bin/ls should have /bin/ as its parent"
    assert $ (map parentName $ findTuple "/bin/ls" tuples) == Just "/bin/"

    log "a leaf file should have correct parent"
    let deepFile = findTuple "/home/user/code/haskell/test.hs" tuples
    let expectedParent = "/home/user/code/haskell/"
    assert $ (map parentName deepFile) == Just expectedParent

childName :: Tuple Path Path -> String
childName = filename <<< snd

parentName :: Tuple Path Path -> String
parentName = filename <<< fst

filterTuple :: String -> Array (Tuple Path Path) -> Array (Tuple Path Path)
filterTuple name = filter (eq name <<< childName)

findTuple :: String -> Array (Tuple Path Path) -> Maybe (Tuple Path Path)
findTuple name = head <<< filterTuple name

countTuple :: String -> Array (Tuple Path Path) -> Int
countTuple str = length <<< filterTuple str
