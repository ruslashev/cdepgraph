import Control.Monad (forM_,when)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import IORead

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        progName <- getProgName
        putStrLn $ "Usage: " ++ progName ++ " <directory>"
        putStrLn   "Start a scan for source files in specified directory."
        putStrLn   ""
        putStrLn   "The resulting GraphViz (neato) code is outputted to stdout,"
        putStrLn   ", so the following way might be preferred:"
        putStrLn $ progName ++ " src/ | neato -T png > out.png"
    else
        startScan $ head args

header :: String
header = unlines [
    "digraph G",
    "{",
    '\t' : "graph [splines=true, overlap=scale]",
    '\t' : "node [shape=box, style=filled, fontname=\"Sans\", fontsize=12.0];" ]

startScan :: String -> IO ()
startScan dir = do
    filesE <- getAbsFileList dir

    putStrLn header

    mainOutput sourceFiles

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

