{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module MazeLearner where

import Control.Monad.State
import qualified Data.Array as A
import System.Random

import BFS
import Environment
import LearningEnvironment
import MazeEnvironment

runLearningWithBase :: IO ([(Int, Reward)], [(Int, Reward)])
runLearningWithBase = do
  gen <- getStdGen
  let lnSt = MazeLearnerState (A.listArray ((0, 0), (15, 4)) (repeat 0.0)) 0.9 gen
  results <- evalStateT (runMazeLearner gameLearningIterations) (lnSt, baseEnvironment)
  return $ (take 10 results, (drop (length results - 10)) results)

runMazeLearner :: MazeLearnerM a -> StateT (MazeLearnerState, MazeGameState) IO a
runMazeLearner (MazeLearnerM action) = action

data MazeLearnerState = MazeLearnerState
  { qTable :: A.Array (Word, Word) Double
  , explorationR :: Double
  , randomGenerator :: StdGen
  }

newtype MazeLearnerM a = MazeLearnerM (StateT (MazeLearnerState, MazeGameState) IO a)
  deriving (Functor, Applicative, Monad)

instance (MonadState (MazeLearnerState, MazeGameState)) MazeLearnerM where
  get = MazeLearnerM get
  put env = MazeLearnerM $ put env

instance MonadIO MazeLearnerM where
  liftIO action = MazeLearnerM (lift action)

instance LearningEnvironment MazeLearnerM where
  type (LearningState MazeLearnerM) = MazeLearnerState
  type (Env MazeLearnerM) = MazeGameM
  liftEnv (MazeGameM action) = do
    (ln, gs) <- get
    (result, gs') <- liftIO $ runStateT action gs 
    put (ln, gs')
    return result
  learnEnv = learnQTable
  chooseActionBrain = chooseActionQTable
  explorationRate = (explorationR . fst) <$> get
  reduceExploration decayRate minEpsilon = do
    lnSt <- getLearningState
    let e = explorationR lnSt
    let newE = max minEpsilon (e * decayRate)
    putLearningState $ lnSt { explorationR = newE }
  getLearningState = fst <$> get
  putLearningState ln' = do
    (_, gs) <- get
    put (ln', gs)

chooseActionQTable :: MazeLearnerM Direction
chooseActionQTable = do
  lnSt <- getLearningState
  let (exploreRoll, gen') = randomR (0.0, 1.0) (randomGenerator lnSt)
  if exploreRoll < explorationR lnSt
    then do
      let (actionRoll, gen'') = randomR (0, 4) gen'
      putLearningState $ lnSt { randomGenerator = gen'' }
      return (toEnum actionRoll)
    else do
      env <- liftEnv get
      let observationIndex = locationToIndex (playerLoc env) (gameGrid env)
      let maxIndex = snd $ snd $ maxScore observationIndex (qTable lnSt)
      putLearningState $ lnSt { randomGenerator = gen' }
      return (toEnum (fromIntegral maxIndex))

learnQTable :: Location -> Direction -> Location -> Reward -> MazeLearnerM ()
learnQTable loc1 direction loc2 (Reward reward) = do
  lnSt <- getLearningState
  let q = qTable lnSt
  grid <- gameGrid <$> liftEnv get
  let actionIndex = fromIntegral . fromEnum $ direction
      observationIndex1 = locationToIndex loc1 grid
      observationIndex2 = locationToIndex loc2 grid
      prediction = q A.! (observationIndex1, actionIndex)
      target = reward + gamma * (fst $ maxScore observationIndex2 q)
      newValue = prediction + learningRate * (target - prediction)
      newQ = q A.// [((observationIndex1, actionIndex), newValue)]
  putLearningState $ lnSt { qTable = newQ }
  where
    gamma = 0.96
    learningRate = 0.81

locationToIndex :: Location -> Grid -> Word
locationToIndex (r, c) grid = fromIntegral $ r * numRows + c
  where
    numRows = (fst . snd $ A.bounds grid) + 1

indexToLocation :: Word -> Grid -> Location
indexToLocation index grid = fromIntegral index `quotRem` numRows
  where
    numRows = (fst . snd $ A.bounds grid) + 1

maxScore :: Word -> A.Array (Word, Word) Double -> (Double, (Word, Word))
maxScore observationIndex q = maximum valuesAndIndices
  where
    indices = (observationIndex,) <$> [0..4]
    valuesAndIndices = (\i -> (q A.! i, i)) <$> indices
