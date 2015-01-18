import Control.Monad (forM_,when)
import Data.Char (toLower)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import IORead

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        progName <- getProgName
        putStrLn $ "Usage: " ++ progName ++ " <directory>"
        putStrLn   "Start a scan for source files in specified directory"
    else
        startScan $ head args

header :: String
header = unlines [
    "digraph G",
    "{",
    '\t' : "graph [splines=true, overlap=scale]",
    '\t' : "node [shape=box, style=filled, fontname=\"Sans\", fontsize=12.0];" ]

sourceFileExtensions, headerFileExtensions :: [String]
sourceFileExtensions = ["cpp", "c", "cxx", "cc", "cp", "c++"]
headerFileExtensions = ["hpp", "h", "hxx", "hh", "hp", "h++"]

includeText :: T.Text
includeText = T.pack "#include"

startScan :: String -> IO ()
startScan dir = do
    filesE <- getFileList dir

    let sourceFiles = filter isSourceFile files

    putStrLn header

    mainOutput sourceFiles

isSourceFile :: String -> Bool
isSourceFile file =
    extension `elem` sourceFileExtensions ++ headerFileExtensions
    where extension = (map toLower . reverse . takeWhile (/= '.') . reverse) file

mainOutput :: [String] -> IO ()
mainOutput [] = putStrLn "}"
mainOutput (file:files) = do
    includes <- getIncludes file
    let relIncludes = map (makeRelative file) includes
    forM_ relIncludes (\ inc -> do
        putStrLn $ "\tnode [" ++ colorizeNode (T.unpack inc) ++ "]"
        putStrLn $ "\t" ++ show file ++ " -> " ++ show inc
        )
    putStrLn $ "\t" ++ show file ++ " [" ++ colorizeNode file ++ "]"

    mainOutput files

getIncludes :: String -> IO [T.Text]
getIncludes file = do
    fileContents <- Tio.readFile file
    let includedFiles =
            [ T.tail $ T.dropWhile (/= ' ') line
            | line <- T.lines fileContents
            , not $ T.null line
            , T.take 8 line == includeText
            ]
    return includedFiles

makeRelative :: String -> T.Text -> T.Text
makeRelative filename include =
    if x == '"' then
        let fileDir = reverse $ tail $ dropWhile (/= '/') $ reverse filename in
        if T.take 3 xs == T.pack "../" then
            let upperDirectory =
                    reverse $ drop 1 $ dropWhile (/= '/') $ reverse fileDir
            in makeRelative (upperDirectory ++ "/")
                            (T.pack "\"" `T.append` T.drop 3 xs)
        else
            if T.take 2 xs == T.pack "./" then
                makeRelative filename (T.pack "\"" `T.append` T.drop 3 xs)
            else
                T.pack fileDir `T.append` T.cons '/' (T.init xs)
    else -- angular brackets or #defined path
        include
    where x  = T.head include
          xs = T.tail include

colorizeNode :: String -> String
colorizeNode inc
    | head inc == '<' = blue
    | fileExt `elem` sourceFileExtensions = yellow
    | fileExt `elem` headerFileExtensions = green
    | otherwise = red
    where fileExt = reverse $ takeWhile (/= '.') $ reverse inc
          blue   = "color=\"#D5EEFB\""
          yellow = "color=\"#F8F8D3\""
          green  = "color=\"#D4F9D4\""
          red    = "color=\"#FAD5D5\""

