module IOWrite
where

import qualified Data.Text as T
import qualified Data.Map as Map

import Processing.Includes

{-

header :: [T.Text]
header =
    [ T.pack "digraph G {"
    , T.pack "graph [overlap=prism,splines=true]"
    , T.pack "node [shape=box,style=filled,fontname=\"Sans\",fontsize=12.0];"
    ]

genOutput :: [SrcFile] -> T.Text
genOutput srcFiles =
    let listOfNodes = nub $ makeListOfNodes srcFiles
        nodeMap = constructMap listOfNodes
    in T.unlines $
        header ++
        assignNodes nodeMap ++
        [T.pack ""] ++
        genRelationships srcFiles nodeMap ++
        [T.pack "}"]

assignNodes :: NodeMap -> [T.Text]
assignNodes nodeMap =
    foldr
        (\ text output ->
            lookupMap text nodeMap `T.append`
            T.pack " [label=\"" `T.append`
            text `T.append`
            T.pack "\"," `T.append`
            colorizeNode text `T.append`
            T.pack "]"
            : output
        ) [] (Map.keys nodeMap)

genRelationships :: [SrcFile] -> NodeMap -> [T.Text]
genRelationships [] _ = []
genRelationships (SrcFile name includes : rest) nodeMap =
    foldl
        (\ output include ->
            lookupMap name nodeMap `T.append`
            T.pack "->" `T.append`
            lookupMap include nodeMap
            : output
        ) [] includes
    ++ genRelationships rest nodeMap

lookupMap :: T.Text -> NodeMap -> T.Text
lookupMap key nodeMap = T.pack $ show $ fromMaybe 0 (Map.lookup key nodeMap)

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

-}

