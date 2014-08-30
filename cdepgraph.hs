import Control.Monad (forM_,when)
import Data.Char (toLower)
import Data.List (foldl')
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import GetFiles

main = do
    args <- getArgs
    if length args /= 1 then do
        progName <- getProgName
        putStrLn $ "Usage: " ++ progName ++ " <directory>"
        putStrLn   "Start a scan for source files in specified directory"
    else
        startScan $ head args

header = unlines [
    "digraph G",
    "{",
    '\t' : "graph [splines=true, overlap=scale]",
    '\t' : "node [shape=box, style=filled, fontname=\"Sans\", fontsize=12.0];" ]

startScan :: String -> IO ()
startScan dir = do
    filesMaybe <- getFileList dir

    when (isNothing filesMaybe) $ do
        putStrLn $ "Failed to open directory \"" ++ dir ++ "\""
        exitFailure

    let files = case filesMaybe of
                Just list -> list
                Nothing   -> [] -- that won't happen. Quit complaining, ghc-mod

    let sourceFiles = filter isSourceFile files

    putStrLn header

    mainOutput sourceFiles

isSourceFile :: String -> Bool
isSourceFile file =
    extension `elem`
         ["cpp", "c", "cxx", "cc", "cp", "c++",
          "hpp", "h", "hxx", "hh", "hp", "h++"]
    where extension = (map toLower . reverse . takeWhile (/= '.') . reverse) file

mainOutput :: [String] -> IO ()
mainOutput [] = putStrLn "}"
mainOutput (file:files) = do
    includes <- getIncludes file
    let relIncludes = getRelIncludes file includes
    forM_ relIncludes (\ inc -> do
        putStrLn $ "\tnode [" ++ colorizeNode (T.unpack inc) ++ "]"
        putStrLn $ "\t" ++ show file ++ " -> " ++ show inc
        )
    putStrLn $ "\t" ++ show file ++ " [" ++ colorizeNode file ++ "]"

    mainOutput files

getIncludes :: String -> IO [T.Text]
getIncludes file = do
    fileContents <- Tio.readFile file
    let ppDirectives = (filter (\ line -> T.head line == '#') .
                        filter (not . T.null) .
                        T.lines) fileContents
    let includedFiles = foldl' (\ acc d ->
            let (kind,arg) = T.break (== ' ') d
            in if kind == T.pack "#include"
               then T.tail arg : acc
               else acc) [] ppDirectives
    return includedFiles

getRelIncludes :: FilePath -> [T.Text] -> [T.Text]
getRelIncludes file = map (makeRelative file)

makeRelative :: FilePath -> T.Text -> T.Text
makeRelative file include =
    if x == '"' then
        let brokenPath = break (== '/') $ reverse file
            fileDir = reverse $ tail $ snd brokenPath
            fileName = reverse $ fst brokenPath in
        if T.take 3 xs == T.pack "../" then
            let upperDirectory =
                    reverse $ drop 1 $ dropWhile (/= '/') $ reverse fileDir
            in makeRelative (upperDirectory ++ "/")
                            (T.pack "\"" `T.append` T.drop 3 xs)
        else
            if T.take 2 xs == T.pack "./" then
                makeRelative file (T.pack "\"" `T.append` T.drop 3 xs)
            else
                T.pack fileDir `T.append` T.cons '/' (T.init xs)
    else -- angular brackets or #defined path
        include
    where x  = T.head include
          xs = T.tail include

colorizeNode :: String -> String
colorizeNode inc
    | head inc == '<' = blue
    | fileExt `elem` ["cpp", "c", "cxx", "cc", "cp", "c++"] = yellow
    | fileExt `elem` ["hpp", "h", "hxx", "hh", "hp", "h++"] = green
    | otherwise = red
    where fileExt = reverse $ takeWhile (/= '.') $ reverse inc
          blue   = "color=\"#D5EEFB\""
          yellow = "color=\"#F8F8D3\""
          green  = "color=\"#D4F9D4\""
          red    = "color=\"#FAD5D5\""

