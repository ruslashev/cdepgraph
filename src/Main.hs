module Main where

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import qualified Data.Text.IO as Tio

import IORead
import Processing
import IOWrite

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1 then do
        progName <- getProgName
        putStrLn $ "Usage: " ++ progName ++ " <directory>"
        putStrLn   "Start a scan for source files in specified directory."
        putStrLn   ""
        putStrLn   "The resulting GraphViz (neato) code is outputted to"
        putStrLn   "stdout, so the following way might be preferred:"
        putStrLn $ "    " ++ progName ++ " src/ | neato -T png > out.png"
    else
        startScan $ head args

startScan :: String -> IO ()
startScan dir = do
    filesE <- getAbsFileList dir
    files <- test filesE

    processed <- process files

    Tio.putStr $ genOutput processed

    where
        test (Left x) = do
            putStrLn x
            exitFailure
        test (Right list) =
            return list
