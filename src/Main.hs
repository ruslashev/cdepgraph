module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import qualified Data.Text.IO as Tio

import IORead
import Processing.Includes
import Processing.Clusters
import IOWrite

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        progName <- getProgName
        putStrLn   "Error: no directory specified."
        putStrLn $ "Usage: " ++ progName ++ " <directory>"
        putStrLn   "Scan for source files in specified directory."
        putStrLn   ""
        putStrLn   "The resulting GraphViz (neato) code is outputted to stdout,"
        putStrLn   "so the following way might be preferred:"
        putStrLn $ "    " ++ progName ++ " src/ | neato -T png > out.png"
    else
        startScan $ head args

-- IORead -> Processing.Includes -> Processing.Clusters -> IOWrite

startScan :: String -> IO ()
startScan dir = do
    filesM <- getAbsFileList dir
    files <- case filesM of
        Just list -> return list
        Nothing -> do
            putStrLn $ "Directory \"" ++ dir ++ "\" doesn't exist"
            exitFailure

    processed <- processIncludes files
    let (clusters,srcFilesI) = clusterize processed

    -- putStrLn "PROCESSED:"
    -- print processed
    putStrLn "CLUSTERS:"
    print clusters

    where
        test (Left err) = do
            putStrLn err
            exitFailure
        test (Right list) =
            return list

