{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module LearningEnvironment where

import Control.Monad
import Control.Monad.IO.Class

import Environment

class (Monad m) => LearningEnvironment m where
  type LearningState m :: *
  type Env m :: * -> *
  liftEnv :: (Env m) a -> m a
  learnEnv :: (EnvironmentMonad (Env m)) => (Observation (Env m)) -> (Action (Env m)) -> (Observation (Env m)) -> Reward -> m ()
  chooseActionBrain :: (EnvironmentMonad (Env m)) => m (Action (Env m))
  explorationRate :: m Double
  reduceExploration :: Double -> Double -> m ()
  getLearningState :: m (LearningState m)
  putLearningState :: (LearningState m) -> m ()

gameLearningLoop :: (Show (Action (Env m)), MonadIO m, LearningEnvironment m, EnvironmentMonad (Env m), RenderableEnvironment (Env m)) => (Int, Reward) -> m (Int, Reward)
gameLearningLoop (i, oldReward) = do
  oldObs <- liftEnv currentObservation
  newAction <- chooseActionBrain
  (newObs, reward, done) <- liftEnv $ stepEnv newAction
  learnEnv oldObs newAction newObs reward
  let newReward = oldReward + reward
  if done
    then return (i, newReward)
    else gameLearningLoop (i + 1, newReward)

gameLearningIterations :: (Show (Action (Env m)), MonadIO m, LearningEnvironment m, EnvironmentMonad (Env m), RenderableEnvironment (Env m)) => m [(Int, Reward)]
gameLearningIterations = forM [1..numEpisodes] $ \i -> do
  liftEnv resetEnv
  when (i `mod` 100 == 99) $ do
    reduceExploration decayRate minEpsilon
  (count, reward) <- gameLearningLoop (0, Reward 0.0)
  return (count, reward)
  where
    numEpisodes = 1000
    decayRate = 0.9
    minEpsilon = 0.01
