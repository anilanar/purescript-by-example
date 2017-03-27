module FileOperations.Internal where

import Prelude
import Data.Array (concat, concatMap, length, zip, (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path (Path, ls, size)
import Data.Tuple (Tuple(..), fst, snd)

size' :: Path -> Int
size' = (fromMaybe 0) <<< size

compareFiles :: (Path -> Path -> Boolean) -> Path -> Path -> Path
compareFiles f x y | f x y = x
compareFiles f x y | otherwise = y

maxPath :: Path -> Path -> Path
maxPath = compareFiles (\x y -> size' x > size' y)

minPath :: Path -> Path -> Path
minPath = compareFiles (\x y -> size' x < size' y)

accMaybe :: forall t. (t -> t -> t) -> (Maybe t -> t -> Maybe t)
accMaybe f = f' where
    f' (Just x) y = Just $ f x y
    f' Nothing y = Just y

asTuples :: Path -> Array (Tuple Path Path)
asTuples root = asTuples' [] $ [root] where
    -- at each step, map all paths from queue to tuples
    -- prepend them to accumulator
    -- then replace queue with concatMap of (ls root) of children
    asTuples' ::
        Array (Tuple Path Path)
        -> Array Path
        -> Array (Tuple Path Path)
    asTuples' acc [] = acc
    asTuples' acc queue = asTuples' (append tuples'' acc) queue' where
        children :: Array (Array Path)
        children =  map ls queue
        queue' :: Array Path
        queue' = concat children
        tuples' :: Array (Tuple Path (Array Path))
        tuples' =  zip queue children
        tuples'' :: Array (Tuple Path Path)
        tuples'' = concatMap zipTuple tuples' where
            zipTuple :: Tuple Path (Array Path) -> Array (Tuple Path Path)
            zipTuple tuple = map (\xs -> Tuple (fst tuple) xs) $ snd tuple



