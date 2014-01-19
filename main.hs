import System.Environment (getArgs, getProgName)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Control.Monad (forM_)
import Data.Char (toLower)

header = unlines [
    "digraph G {",
    "\tgraph [splines=true,overlap=scale]",
    "\tnode [shape=box,style=filled,fontname=\"Sans\",fontsize=12.0];"
    ]

main = do
    args <- getArgs
    if length args /= 1 then do
        progName <- getProgName
        putStr $ unlines [
            "Usage: " ++ progName ++ " <directory>",
            "Start a scan for source files in specified directory"]
    else
        startScan $ head args

startScan :: FilePath -> IO ()
startScan directory = do
    let dir = reverse $ dropWhile (== '/') $ reverse directory

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
    fileContents <- readFile file
    return $ filter (\ x -> not (null x) && head x == '#') $ lines fileContents

getFileList :: FilePath -> IO ([String])
getFileList dir = do
    folderContents <- getDirectoryContents dir
    let relativeFolderContents =
            map ((dir ++ "/") ++) $ filter (\ x -> head x /= '.') folderContents
    fileList <- getFiles dir relativeFolderContents
    return fileList
    where
    getFiles :: FilePath -> [FilePath] -> IO ([String])
    getFiles dir (x:xs) = do
        isDir <- doesDirectoryExist x
        if isDir then do
            files <- getFileList x
            rest  <- getFiles dir xs
            return $ files ++ rest
        else do
            rest <- getFiles dir xs
            return $ x : rest
    getFiles _ [] = return []

