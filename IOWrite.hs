module IOWrite (genOutput)
where

import qualified Data.Text as T
import qualified Data.Map as Map
import Data.List (nub)

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
    in T.unlines $
        header ++
        assignNodes listOfNodes

makeListOfNodes :: [SrcFile] -> [T.Text]
makeListOfNodes [] = []
makeListOfNodes (SrcFile name includes : rest) =
    name : includes ++ makeListOfNodes rest

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

