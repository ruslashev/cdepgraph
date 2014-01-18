import System.Environment (getArgs, getProgName)
import System.Directory

source = unlines [
    "digraph G {",
    "node [shape=box/*,fixedsize=true,width=2.0*/]; a; b; c;",
    "a -> b;",
    "b -> c;",
    "overlap=false",
    "}"]

main = do
    args <- getArgs
    progName <- getProgName
    -- if length args /= 1 then
    --     putStr $ unlines [
    --           "Usage: " ++ progName ++ " <directory>"
    --         , "Start a scan for source files in specified directory"
    --         ]
    -- else
    startScan $ "link-to-dir" -- head args

startScan :: FilePath -> IO ()
startScan dir = do
    files <- getFileList dir
    mapM putStrLn files
    return ()

getFileList :: FilePath -> IO ([FilePath])
getFileList dir = do
    folderContents <- getDirectoryContents dir
    let relativeFolderContents =
            map ((dir ++ "/") ++) $ filter (\ x -> head x /= '.') folderContents
    fileList <- getFiles dir relativeFolderContents
    return fileList
    where
    getFiles :: FilePath -> [FilePath] -> IO ([FilePath])
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

