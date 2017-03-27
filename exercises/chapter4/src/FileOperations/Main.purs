module FileOperations where

import Data.Array (concatMap, filter, foldl, head, (:))
import Data.Maybe (Maybe(Nothing))
import Data.Path (Path, filename, isDirectory, ls, root)
import Data.Tuple (fst, snd)
import FileOperations.Internal (accMaybe, asTuples, maxPath, minPath)
import Prelude (map, not, ($), (<<<), (==))

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

onlyFiles :: Path -> Array Path
onlyFiles = (filter $ not <<< isDirectory) <<< allFiles

findLargest :: Path -> Maybe Path
findLargest = foldl (accMaybe maxPath) Nothing <<< onlyFiles

findSmallest :: Path -> Maybe Path
findSmallest = foldl (accMaybe minPath) Nothing <<< onlyFiles

whereIs :: String -> Maybe Path
-- list tuples of parent/child paths, filter them, get head, print parent
whereIs name = map fst tuple where
    tuple = head $ filter ((==) name <<< filename <<< snd) $ asTuples root
