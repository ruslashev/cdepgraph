module IOWrite
where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List (nub)
import Data.Maybe (fromMaybe)

import Processing

type NodeMap = Map.Map T.Text Int

header :: [T.Text]
header =
    [ T.pack "digraph G"
    , T.pack "{"
    , T.pack "graph [splines=true, overlap=scale]"
    , T.pack "node [shape=box, style=filled, fontname=\"Sans\", fontsize=12.0];"
    ]

genOutput :: [SrcFile] -> T.Text
genOutput srcFiles =
    let listOfNodes = nub $ makeListOfNodes srcFiles
        nodeMap = constructMap listOfNodes
    in T.unlines $
        header ++
        assignNodes listOfNodes ++
        [T.pack ""] ++
        genRelationships srcFiles nodeMap ++
        [T.pack "}"]

makeListOfNodes :: [SrcFile] -> [T.Text]
makeListOfNodes [] = []
makeListOfNodes (SrcFile name includes : rest) =
    name : includes ++ makeListOfNodes rest

constructMap :: [T.Text] -> NodeMap
constructMap listOfNodes = Map.fromList $ zip listOfNodes [1..]

assignNodes :: [T.Text] -> [T.Text]
assignNodes listOfNodes =
    foldr
        (\ (idx, text) output ->
            T.pack (show idx ++ "\t [label=\"") `T.append`
            text `T.append`
            T.pack "\", " `T.append`
            colorizeNode text `T.append`
            T.pack "]"
            : output
        ) [] (zip [1..] listOfNodes)

colorizeNode :: T.Text -> T.Text
colorizeNode text
    | T.head text == '<'         = blue
    | ext `elem` sourceFileExtsT = yellow
    | ext `elem` headerFileExtsT = green
    | otherwise                  = red
    where ext = getExtensionT text
          blue   = T.pack "color=\"#D5EEFB\""
          yellow = T.pack "color=\"#F8F8D3\""
          green  = T.pack "color=\"#D4F9D4\""
          red    = T.pack "color=\"#FAD5D5\""

genRelationships :: [SrcFile] -> NodeMap -> [T.Text]
genRelationships [] _ = []
genRelationships (SrcFile name includes : rest) nodeMap =
    foldl
        (\ output include ->
            lookup name `T.append`
            T.pack " -> " `T.append`
            lookup include
            : output
        ) [] includes
    ++ genRelationships rest nodeMap
    where lookup :: T.Text -> T.Text
          lookup key = T.pack $ show $ fromMaybe 0 (Map.lookup key nodeMap)

