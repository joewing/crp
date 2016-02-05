
module Main where

import CRP (ModelParams, create, run, add)
import CSVFileReader
import qualified Data.Map as Map

readSamples :: ModelParams -> String -> IO ModelParams
readSamples params filename = do
    lines <- readCSV filename
    return $ helper params 1 lines
    where
        helper params i [] = params
        helper params i (x:xs) =
            let row = Map.fromList $ zip [1 .. ] (map read x) in
            helper (add params i row) (i + 1) xs

main :: IO ()
main = do
    let alpha = 5
    let beta = 1
    let crp = create alpha beta
    crp' <- readSamples crp "samples.csv"
    let d = run crp' 100
    putStrLn $ show $ d
