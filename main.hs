
module Main where

import CRP (create, run, add)
import qualified Data.Map as Map

samples :: [(Int, Map.Map Int Int)]
samples = [
        --                red     green   white    brown
        (1, Map.fromList [(1, 2), (2, 2), (3, 3), (4, 3)]), -- Normal
        (2, Map.fromList [(1, 3), (2, 2), (3, 3), (4, 3)]), -- Normal
        (3, Map.fromList [(1, 5), (2, 4), (3, 0), (4, 0)]), -- Christmas
        (4, Map.fromList [(1, 4), (2, 5), (3, 0), (4, 0)]), -- Christmas
        (5, Map.fromList [(1, 4), (2, 0), (3, 5), (4, 0)]), -- Fourth
        (6, Map.fromList [(1, 5), (2, 0), (3, 4), (4, 0)])  -- Fourth
    ]


main :: IO ()
main = do
    let alpha = 5
    let beta = 1
    let crp = create alpha beta
    let crp' = foldl (\a (i, s) -> add a i s) crp samples
    let d = run crp' 100
    putStrLn $ show $ d
