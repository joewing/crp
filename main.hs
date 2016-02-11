
module Main where

import CRP (ModelParams, create, run, add)
import CSVFileReader
import qualified Data.Map as Map
import System.Environment

readSamples :: ModelParams -> String -> IO ModelParams
readSamples params filename = do
    (header:lines) <- readCSV filename
    return $ helper params lines
    where
        helper params [] = params
        helper params (x:xs) =
            let (name:values) = x
                row = Map.fromList $ zip [1 .. ] (map read values) in
            helper (add params name row) xs

parseArguments :: [String] -> Map.Map String String
parseArguments args = parse args Map.empty
    where
        parse (key:value:rest) m = parse rest $ Map.insert key value m
        parse (key:[]) m = Map.insert "file" key m
        parse [] m = m

main :: IO ()
main = do
    args <- getArgs
    let argMap = parseArguments args
    let
        alpha = read $ Map.findWithDefault "5" "-alpha" argMap
        beta = read $ Map.findWithDefault "1" "-beta" argMap
        iters = read $ Map.findWithDefault "100" "-iters" argMap
        file = Map.findWithDefault "samples.csv" "file" argMap
    if file == "-help" then do
        putStrLn "usage: crp [options] <samples.csv>"
        putStrLn "options:"
        putStrLn "  -alpha <value:5>"
        putStrLn "  -beta <value:1>"
        putStrLn "  -iterations <value:100>"
    else do
        let crp = create alpha beta
        crp' <- readSamples crp file
        putStrLn $ show $ run crp' iters
