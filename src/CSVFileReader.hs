
module CSVFileReader(
    readCSV
) where

import System.IO

readLines :: Handle -> IO [String]
readLines handle = do
    eof <- hIsEOF handle
    if eof then return []
    else  do
        line <- hGetLine handle
        remaining <- readLines handle
        return $ line:remaining

parseLine :: String -> [String]
parseLine = foldr update [[]]
    where
        update ch [] = if ch == ',' then ["", [ch]] else [[ch]]
        update ch (x:xs) = if ch == ',' then []:(x:xs) else (ch:x):xs

readCSV :: String -> IO [[String]]
readCSV filename = do
    handle <- openFile filename ReadMode    
    lines <- readLines handle
    return $ map parseLine lines
