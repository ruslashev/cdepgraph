module Processing.Clusters
where

import Data.List (nub, elemIndex)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.FilePath.Posix (splitFileName,splitDirectories)

import Processing.Includes

data Directory = Folder String Directory | File String deriving (Show)
data SrcFileI = SrcFileI { nameI :: Int
                         , includesI :: [Int]
                         } deriving (Show)

-- clusterize :: [SrcFile] -> [Directory]
-- clusterize srcFiles =
--     let lookupList = nub $ sort $ srcFilesToList srcFiles
--         srcFilesI = makeSrcFilesI lookupList srcFiles
--     in

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

breakPath :: String -> [String]
breakPath = filter (/= "/") . splitDirectories

textToDirectory :: String -> Directory
textToDirectory str = aux $ breakPath str --(T.unpack str)
    where aux (x:[]) = File x
          aux (x:xs) = Folder x (aux xs)

