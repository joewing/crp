
module Main where

import CRP (ModelParams, create, run, add)
import CSVFileReader
import qualified Data.Map as Map

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

main :: IO ()
main = do
    let alpha = 5
    let beta = 1
    let crp = create alpha beta
    crp' <- readSamples crp "samples.csv"
    let d = run crp' 100
    putStrLn $ show $ d
