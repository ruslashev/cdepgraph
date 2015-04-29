module Processing.Clusters (clusterize)
where

-- Processing.Clusters, returns clusterized list of includes and list
-- of indexed source files

import Data.List (elemIndex, nub, partition, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.FilePath.Posix (splitFileName,splitDirectories)
import Debug.Trace

import Processing.Includes

-- A cluster is basically a thing to encapsulate all the files from one
-- directory under one container. That's for includable local files,
-- the ones that are used as (#include "header.hpp"). Since all includes
-- are turned to clusters, other headers are defined as "SystemInclude"
data Cluster = SystemInclude T.Text
             | Folder T.Text [Cluster]
             | File T.Text
             deriving (Show)

data SrcFileI = SrcFileI { nameI :: Int
                         , includesI :: [Int]
                         } deriving (Show)

clusterize :: [SrcFile] -> ([Cluster], [SrcFileI])
clusterize srcFiles =
    let listOfFilesAndIncludes = nub $ sort $ srcFilesToList srcFiles
        (fileIncludes,systemIncludes) =
            partition (\ x -> T.head x == '/') listOfFilesAndIncludes
        fileIncsRepeatsRemoved = removeLeadingRepeats fileIncludes
        clusters = makeClusters fileIncsRepeatsRemoved systemIncludes
        srcFilesI = makeSrcFilesI listOfFilesAndIncludes srcFiles
    in (clusters, srcFilesI)

srcFilesToList :: [SrcFile] -> [T.Text]
srcFilesToList [] = []
srcFilesToList (SrcFile name includes : rest) =
    name : includes ++ srcFilesToList rest

removeLeadingRepeats :: [T.Text] -> [T.Text]
removeLeadingRepeats list@(x:_) =
    if all (\ y -> T.head x == T.head y) list
    then removeLeadingRepeats (map T.tail list)
    else list

makeClusters :: [T.Text] -> [T.Text] -> [Cluster]
makeClusters fileIncludes sysIncludes =
    -- map SystemInclude sysIncludes ++
    let clusters = filesToClusters fileIncludes
    in mergeCommonFolders clusters

filesToClusters :: [T.Text] -> [Cluster]
filesToClusters =
    map (strToCluster . breakPath)
    where
        strToCluster :: [T.Text] -> Cluster
        strToCluster (x:[]) = File x
        strToCluster (x:xs) = Folder x [strToCluster xs]
        breakPath :: T.Text -> [T.Text]
        breakPath = map T.pack . filter (/= "/") . splitDirectories . T.unpack

mergeCommonFolders :: [Cluster] -> [Cluster]
mergeCommonFolders (x:xs) =
    case x of
        Folder name contents ->
            let (sameFolders,everythingElse) = getTheseFolders name xs
                folder = mergeToFolder sameFolders
            in folder : everythingElse
    where getTheseFolders :: T.Text -> Cluster -> ([Cluster],[Cluster])
          getTheseFolders name clusters = -- partition?
              (filter (isFolderOfThisName name) clusters,
               filter (not . isFolderOfThisName name) clusters)
          isFolderOfThisName :: T.Text -> Cluster -> Bool
          isFolderOfThisName givenName (Folder testName _) = testName == givenName
          isFolderOfThisName _ _ = False
          mergeToFolder :: [Cluster] -> Cluster
          mergeToFolder [] = []
          mergeToFolder (f:fs) =
              let (Folder name firstFolderContents) = f
              in Folder name $ firstFolderContents ++ getContents fs
          getContents :: [Cluster] -> [T.Text]
          getContents [] = []
          getContents (Folder _ contents : rest) = contents ++ getContents rest

makeSrcFilesI :: [T.Text] -> [SrcFile] -> [SrcFileI]
makeSrcFilesI _ [] = []
makeSrcFilesI lookupList (SrcFile name includes : rest) =
    SrcFileI (lookupI lookupList name) (map (lookupI lookupList) includes) :
        makeSrcFilesI lookupList rest

lookupI :: [T.Text] -> T.Text -> Int
lookupI hay needle = fromMaybe 0 (elemIndex needle hay)

