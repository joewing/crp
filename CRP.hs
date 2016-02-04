module CRP where

import Control.Monad.State
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List
import System.Random

type SparseVector a = Map.Map Int a
type SparseFunction a = SparseVector a -> SparseVector a
type SparseFunction2 a = SparseVector a -> SparseFunction a

type Sample = SparseVector Int          -- item -> count
type Cluster = SparseVector Int         -- topic -> count

data ModelParams = ModelParams {
    seed        :: Int,
    alpha       :: Double,
    beta        :: Double,
    samples     :: Map.Map Int Sample,  -- sample -> (item -> count)
    topicCount  :: Int
}

data ModelData = ModelData {
    clusters    :: Map.Map Int Cluster,     -- cluster -> (item -> count)
    mapping     :: Map.Map Int Int,         -- sample -> cluster
    generator   :: StdGen
}

type CRPState a = ReaderT ModelParams (State ModelData) a

instance Show ModelData where
    show d = Map.foldWithKey (\k v acc ->
            acc ++ show k ++ ": " ++ show v ++ "\n"
        ) "" $ mapping d

topics :: SparseVector Sample -> Set.Set Int
topics = Map.fold sampleTopics Set.empty
    where
        sampleTopics sample acc =
            Set.union acc $ Map.keysSet $ sample

countTopics :: SparseVector Sample -> Int
countTopics = Set.size . topics

-- Get the number of occurances of each topic.
topicCounts :: ModelParams -> SparseVector Int
topicCounts params = Map.foldl vecAdd Map.empty $ samples params

-- Create a new CRP model.
create :: Double -> Double -> ModelParams
create alpha beta = ModelParams {
    seed = 1,
    alpha = alpha,
    beta = beta,
    topicCount = 0,
    samples = Map.empty
}

-- Add a sample to the model.
addSample :: ModelParams -> Int -> Sample -> ModelParams
addSample params key sample =
    params { samples = Map.insert key sample $ samples params }

-- Map an operation over a sparse vector.
vecMap :: (Eq a, Num a) => (a -> a -> a) -> SparseFunction2 a
vecMap f a b =
    Set.fold combine Map.empty $ Set.union (Map.keysSet a) (Map.keysSet b)
    where
        combine key m =
            let left = Map.findWithDefault 0 key a
                right = Map.findWithDefault 0 key b
                value = f left right in
            if value == 0 then m
            else Map.insert key value m

-- Add two sparse vectors.
vecAdd :: (Eq a, Num a) => SparseFunction2 a
vecAdd = vecMap (+)

-- Subtract one sparse vector from another.
vecSubtract :: (Eq a, Num a) => SparseFunction2 a
vecSubtract = vecMap (-)

-- Apply a function to a cluster.
clusterApply :: SparseFunction2 Int -> ModelParams
                -> ModelData -> Int -> Int -> ModelData
clusterApply f p d c s =
    let sample = Map.findWithDefault Map.empty s $ samples p
        oldClusters = clusters d
        cluster = Map.findWithDefault Map.empty c oldClusters
        newCluster = f cluster sample in
    if Map.size newCluster > 0 then
        d { clusters = Map.insert c newCluster oldClusters }
    else
        d { clusters = Map.delete c oldClusters }

-- Remove a sample from a cluster.
clusterRemove :: Int -> Int -> CRPState ()
clusterRemove cid sid = do
    d <- get
    p <- ask
    let d2 = clusterApply vecSubtract p d cid sid
    put $ d2 { mapping = Map.delete sid (mapping d2) }

-- Insert a sample to the specified cluster.
-- This assumes that the sample is not inserted somewhere else.
insertSample :: Int -> Int -> CRPState ()
insertSample cid sid = do
    d <- get
    p <- ask
    let d2 = clusterApply vecAdd p d cid sid
    put $ d2 { mapping = Map.insert sid cid (mapping d2) }

-- Remove a sample from its current cluster.
removeSample :: Int -> CRPState ()
removeSample sid = do
    d <- get
    case Map.lookup sid $ mapping d of
        Just oc -> clusterRemove oc sid
        Nothing -> return ()

-- Run the model.
run :: ModelParams -> Int -> ModelData
run params iters =
    let params' = params { topicCount = countTopics $ samples params }
        d = ModelData {
            clusters = Map.empty,
            mapping = Map.empty,
            generator = mkStdGen $ seed params
        }
    in
    fst $ runState (runReaderT runner params') d
    where
        runner = do
            p <- ask
            mapM_ (\i -> insertSample 0 i) (Map.keys $ samples p)
            best <- get
            foldM clusterIter best [1 .. iters]

-- Select a sample to move.
selectSample :: CRPState Int
selectSample = do
    p <- ask
    d <- get
    let range = (0, (Map.size $ samples p) - 1)
    let (index, g) = randomR range $ generator d
    let sid = Map.keys (samples p) !! index
    put $ d { generator = g }
    return sid

-- Draw a random number.
drawDouble :: CRPState Double
drawDouble = do
    d <- get
    let (value, g) = random $ generator d
    put $ d { generator = g}
    return value

-- Select a new cluster for a sample.
selectCluster :: Int -> CRPState Int
selectCluster sid = do
    p <- ask
    draw <- drawDouble
    let s = Map.findWithDefault Map.empty sid (samples p)
    probs <- assignmentProbs s
    let (cid, _) = foldl check (-1, draw) $ Map.assocs probs
    return $ max 0 cid
    where
        check (found, total) (cid, prob) =
            let newProb = total - prob in
            if found < 0 && newProb <= 0.0 then (cid, 0.0)  -- Newly found
            else if found < 0 then (found, newProb)         -- Not found yet
            else (found, total)                             -- Already found

-- Perform a sinle iteration.
clusterIter :: ModelData -> Int -> CRPState ModelData
clusterIter best _ = do
    sid <- selectSample
    removeSample sid
    cid <- selectCluster sid
    insertSample cid sid
    pBest <- logPostProb best
    pCurrent <- get >>= logPostProb
    if pCurrent > pBest then get >>= return
    else return best

-- Create a histogram.
createHistogram :: [Int] -> SparseVector Int
createHistogram = foldl update Map.empty
    where
        update vec index =
            let value = 1 + Map.findWithDefault 0 index vec in
            Map.insert index value vec

-- Get the size of each cluster.
clusterSizes :: ModelParams -> ModelData -> SparseVector Int
clusterSizes params = createHistogram . Map.elems . mapping

-- Get the number of clusters of each size.
-- Returns a mapping from cluster size to count.
clusterSizeCounts :: ModelParams -> ModelData -> SparseVector Int
clusterSizeCounts params = createHistogram . Map.elems . clusterSizes params

-- Determine the probability of the current clustering.
logProbClustering :: ModelParams -> ModelData -> Double
logProbClustering params d =
    let counts = clusterSizeCounts params d in
    Map.foldWithKey update 0.0 counts
    where
        update size count partial =
            let fcount = fromIntegral count
                fsize = fromIntegral size in
            partial + fcount * log fsize + logGamma (fcount + 1.0)

logPostProb :: ModelData -> CRPState Double
logPostProb d = do
    params <- ask
    let cs = clusters d
        b = beta params
        tc = fromIntegral $ topicCount params
        cc = fromIntegral $ Map.size cs
        likelihood = sum $ map (\c -> computeC params c) $ Map.elems cs
        part = cc * (logGamma b * tc - logGamma (b * tc))
        lpc = logProbClustering params d
    return $ (likelihood - part) + lpc

-- Determine the probability of assignment to each table.
assignmentProbs :: Sample -> CRPState (SparseVector Double)
assignmentProbs s = do
    params <- ask
    d <- get
    let weights = clusterWeights params d s
        maxWeight = maximum $ Map.elems weights
        unlogged = Map.map (\x -> exp $ x - maxWeight) weights
        total = sum $ Map.elems unlogged
    return $ Map.map (/ total) unlogged

-- Determine the weight of each cluster for a sample.
-- This assumes that the model data does not include the sample.
-- This includes a slot for the new cluster.
clusterWeights :: ModelParams -> ModelData -> Sample -> SparseVector Double
clusterWeights params d s =
    let newClusterId = head [k | k <- [0 .. ] \\ Map.keys (clusters d)]
        tc = fromIntegral $ topicCount params
        lpnew = log $ (alpha params) / (alpha params + tc - 1)
        newClusterProb = logClusterProb params s Map.empty + lpnew
        existing = Map.mapWithKey weight (clusters d) in
    Map.insert newClusterId newClusterProb existing
    where
        count = fromIntegral $ Map.size (clusters d)
        sizes = clusterSizes params d
        tc = fromIntegral $ topicCount params
        weight cid c =
            let size = fromIntegral $ Map.findWithDefault 0 cid sizes
                mixing = size / (alpha params + tc - 1) in
            logClusterProb params s c + log mixing

-- Determine the log of the probability of a sample being in a cluster.
logClusterProb :: ModelParams -> Sample -> Cluster -> Double
logClusterProb params s c =
    let combined = vecAdd s c in
    (computeC params combined) - (computeC params c)

-- Compute "C" for a cluster.
computeC :: ModelParams -> Cluster -> Double
computeC params vec =
    let partSum = Map.fold sumValuesFunc 0.0 vec
        partLogGamma = Map.fold sumLogGammaFunc 0.0 vec
        size = fromIntegral $ Map.size vec
        zeroCount = fromIntegral $ topicCount params - size
        zeroSum = zeroCount * beta params
        totalSum = partSum + zeroSum
        zeroLogGamma = zeroCount * (logGamma $ beta params)
        totalLogGamma = partLogGamma + zeroLogGamma in
    totalLogGamma - logGamma totalSum
    where
        sumValuesFunc v a = a + fromIntegral v + beta params
        sumLogGammaFunc v a = a + logGamma (fromIntegral v + beta params)

-- Log Gamma implemented as a partial expansion of a taylor series.
logGamma :: Double -> Double
logGamma x =
   let tmp = (x - 0.5) * (log (x + 4.5)) - (x + 4.5) in
   let ser = 1.0 + 76.18009173 / (x + 0.0) - 86.50532033 / (x + 1.0)
           + 24.01409822 / (x + 2.0) - 1.231739516 / (x + 3.0)
           + 0.00120858003 / (x + 4.0) - 0.00000536382 / (x + 5.0) in
   tmp + log (ser + sqrt (2.0 + pi))
