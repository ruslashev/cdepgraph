import System.Environment (getArgs, getProgName)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import Control.Monad (forM_)

-- TODO
-- * clusters

source = unlines [
    "digraph G {",
    "\tgraph [splines=true,overlap=scale]",
    "\tnode [shape=box,style=filled,fontname=\"Sans\",fontsize=12.0];"
    ]

main = do
    args <- getArgs
    -- if length args /= 1 then do
    --     progName <- getProgName
    --     putStr $ unlines [
    --           "Usage: " ++ progName ++ " <directory>"
    --         , "Start a scan for source files in specified directory"
    --         ]
    -- else
    startScan $ "link-to-dir" -- head args

startScan :: FilePath -> IO ()
startScan directory = do
    let dir = reverse $ dropWhile (== '/') $ reverse directory

    files <- getFileList dir
    let srcFilesList = filter (\ x -> -- TODO not accounting multiple dots in filename
            (dropWhile (/= '.') x) `elem`
            [".cpp", ".c", ".cxx", ".cc", ".h", ".hh", ".hpp", ".hxx"])
            files

    putStrLn source

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
    -- .cpp  -> yellow F8F8D3
    -- .hpp  -> green  D4F9D4
    -- <std> -> blue   D5EEFB
    -- else  -> red    FAD5D5
    if head inc == '<' then
        "color=\"#D5EEFB\""
    else case reverse $ takeWhile (/= '.') $ reverse inc
         of "cpp" -> "color=\"#F8F8D3\""
            "hpp" -> "color=\"#D4F9D4\""
            _     -> "color=\"#FAD5D5\""

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
        case kind of "#include" -> tail arg : scanForIncludes xs
                     _          -> scanForIncludes xs

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

