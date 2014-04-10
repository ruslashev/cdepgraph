import Control.Monad (forM_)
import Control.Exception (catch)
import Data.Char (toLower)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

header = unlines [
    "digraph G",
    "{",
    '\t' : "graph [splines=true, overlap=scale]",
    '\t' : "node [shape=box, style=filled, fontname=\"Sans\", fontsize=12.0];"]

main = do
    args <- getArgs
    if length args /= 1 then do
        progName <- getProgName
        putStrLn $ "Usage: " ++ progName ++ " <directory>"
        putStrLn $ "Start a scan for source files in specified directory"
    else
        startScan $ head args

startScan :: String -> IO ()
startScan dir = do
    files <- getFileList dir
    let srcFilesList = filter (\ file ->
            (map toLower $ reverse $ takeWhile (/= '.') $ reverse file) `elem`
            ["cpp", "c", "cxx", "cc", "cp", "c++",
             "hpp", "h", "hxx", "hh", "hp", "h++"])
            files

    putStrLn header

    forM_ srcFilesList (\ file -> do
            stripped <- getPPdirectives file
            let includes = scanForIncludes stripped
            let relIncludes = getRelIncludes file includes
            forM_ relIncludes (\ inc -> do
                putStrLn $ "\tnode [" ++ colorizeNode inc ++ "]"
                putStrLn $ "\t" ++ show file ++ " -> " ++ show inc
                )
            putStrLn $ "\t" ++ show file ++ " [" ++ colorizeNode file ++ "]"
            )

    putStrLn "}"

colorizeNode :: String -> String
colorizeNode inc =
    if head inc == '<' then blue
    else if fileExt `elem` ["cpp", "c", "cxx", "cc", "cp", "c++"] then yellow
    else if fileExt `elem` ["hpp", "h", "hxx", "hh", "hp", "h++"] then green
    else red
    where fileExt = reverse $ takeWhile (/= '.') $ reverse inc
          blue   = "color=\"#D5EEFB\""
          yellow = "color=\"#F8F8D3\""
          green  = "color=\"#D4F9D4\""
          red    = "color=\"#FAD5D5\""

getRelIncludes :: String -> [String] -> [String]
getRelIncludes _ [] = []
getRelIncludes file incs@(x:xs) = (makeRelative file x) : getRelIncludes file xs

makeRelative :: String -> String -> String
makeRelative file inc@(x:xs) =
    if x == '"' then
        let brokenPath = break (== '/') $ reverse file
            path = reverse $ drop 1 $ snd brokenPath
            fileName = reverse $ fst brokenPath in
        if take 3 xs == "../" then
            let backDir = reverse $ drop 1 $ dropWhile (/= '/') $ reverse path
            in makeRelative (backDir ++ "/" ++ fileName) ("\"" ++ drop 3 xs)
        else
            if take 2 xs == "./" then
                makeRelative file ("\"" ++ drop 3 xs)
            else
                path ++ "/" ++ init xs
    else -- #defined path or angular brackets
        inc

scanForIncludes :: [String] -> [String]
scanForIncludes [] = []
scanForIncludes (x:xs) =
    let (kind,arg) = break (== ' ') x in
        if kind == "#include" then
            tail arg : scanForIncludes xs
        else
            scanForIncludes xs

getPPdirectives :: FilePath -> IO ([String])
getPPdirectives file = do
    fileContents <- Tio.readFile file
    return $ map T.unpack $ filter (\ x -> not (T.null x) && T.head x == '#') $ T.lines fileContents

getFileList :: String -> IO ([String])
getFileList dir = do
    let dirWithSlash = if (last dir) == '/' then dir else dir ++ "/"
    folderContents <- (getDirectoryContents dirWithSlash) `catch` handler
    let absoluteFolderContents =
            map (dirWithSlash ++) $ filter (\ x -> head x /= '.') folderContents
    fileList <- openDirs absoluteFolderContents
    return fileList
    where
    openDirs :: [FilePath] -> IO ([String])
    openDirs [] = return []
    openDirs (x:xs) = do
        isDir <- doesDirectoryExist x
        fileList <- if isDir then getFileList x
                    else return [x]
        rest <- openDirs xs
        return $ fileList ++ rest
    handler :: IOError -> IO [FilePath]
    handler _ = do
        putStrLn $ "Failed to open directory \"" ++ dir ++ "\""
        exitFailure
        return [""]

