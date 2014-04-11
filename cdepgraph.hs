import Control.Monad (forM_)
import Control.Exception (catch)
import Data.Char (toLower)
import Data.List (foldl')
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
    let srcFilesList = filter isSrcFile files

    putStrLn header

    forM_ srcFilesList (\ file -> do
            includes <- getIncludes file
            let relIncludes = getRelIncludes file includes
            forM_ relIncludes (\ inc -> do
                putStrLn $ "\tnode [" ++ colorizeNode (T.unpack inc) ++ "]"
                putStrLn $ "\t" ++ show file ++ " -> " ++ show inc
                )
            putStrLn $ "\t" ++ show file ++ " [" ++ colorizeNode file ++ "]"
            )

    putStrLn "}"
    where isSrcFile file =
              (map toLower . reverse . takeWhile (/= '.') . reverse) file
              `elem` ["cpp", "c", "cxx", "cc", "cp", "c++",
                      "hpp", "h", "hxx", "hh", "hp", "h++"]

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

getRelIncludes :: FilePath -> [T.Text] -> [T.Text]
getRelIncludes file includes = map (makeRelative file) includes

makeRelative :: FilePath -> T.Text -> T.Text
makeRelative file include =
    if x == '"' then
        let brokenPath = break (== '/') $ reverse file
            fileDir = reverse $ tail $ snd brokenPath
            fileName = reverse $ fst brokenPath in
        if T.take 3 xs == T.pack "../" then
            let upperDirectory =
                    reverse $ drop 1 $ dropWhile (/= '/') $ reverse fileDir
            in makeRelative (upperDirectory ++ "/" ++ fileName) -- TODO
                            ((T.pack "\"") `T.append` (T.drop 3 xs))
        else
            if T.take 2 xs == (T.pack "./") then
                makeRelative file ((T.pack "\"") `T.append` (T.drop 3 xs))
            else
                (T.pack fileDir) `T.append` (T.cons '/' (T.init xs))
    else -- angular brackets or #defined path
        include
    where x  = T.head include
          xs = T.tail include

getIncludes :: FilePath -> IO ([T.Text])
getIncludes file = do
    fileContents <- Tio.readFile file
    let ppDirectives = (filter (\ line -> T.head line == '#') .
                        filter (\ line -> not $ T.null line) .
                        T.lines) fileContents
    let includedFiles = foldl' (\ acc d ->
            let (kind,arg) = T.break (== ' ') d
            in if kind == (T.pack "#include")
            then (T.tail arg) : acc
            else acc) [] ppDirectives
    return includedFiles

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

