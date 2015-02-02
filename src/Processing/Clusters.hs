module Processing.Clusters
where

import Data.List (elemIndex, nub, partition, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.FilePath.Posix (splitFileName,splitDirectories)

import Processing.Includes

data Cluster = SystemInclude T.Text
             | Folder T.Text Cluster
             | File T.Text
             deriving (Show)
data SrcFileI = SrcFileI { nameI :: Int
                         , includesI :: [Int]
                         } deriving (Show)

clusterize :: [SrcFile] -> ([Cluster], [SrcFileI])
clusterize srcFiles =
    let filesAndIncludes = nub $ sort $ srcFilesToList srcFiles
        (fileIncludes,systemIncludes) =
            partition (\ x -> T.head x == '/') filesAndIncludes
        lookupList = systemIncludes ++ fileIncludes
        srcFilesI = makeSrcFilesI lookupList srcFiles
        fileIncsRepeatsRemoved = removeRepeats fileIncludes
        clusters = makeClusters fileIncsRepeatsRemoved systemIncludes
    in (clusters, srcFilesI)

srcFilesToList :: [SrcFile] -> [T.Text]
srcFilesToList [] = []
srcFilesToList (SrcFile name includes : rest) =
    name : includes ++ srcFilesToList rest

makeSrcFilesI :: [T.Text] -> [SrcFile] -> [SrcFileI]
makeSrcFilesI _ [] = []
makeSrcFilesI lookupList (SrcFile name includes : rest) =
    SrcFileI (lookup name) (map lookup includes) : makeSrcFilesI lookupList rest
        where lookup :: T.Text -> Int
              lookup key = fromMaybe 0 (elemIndex key lookupList)

removeRepeats :: [T.Text] -> [T.Text]
removeRepeats list@(x:_) =
    if all (\ y -> T.head x == T.head y) list
    then removeRepeats (map T.tail list)
    else list

makeClusters :: [T.Text] -> [T.Text] -> [Cluster]
makeClusters fileIncludes sysIncludes =
    map fileToCluster fileIncludes ++
    map SystemInclude sysIncludes

fileToCluster :: T.Text -> Cluster
fileToCluster str = aux $ breakPath str
    where aux (x:[]) = File (T.pack x)
          aux (x:xs) = Folder (T.pack x) (aux xs)
          breakPath = filter (/= "/") . splitDirectories . T.unpack

