{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Environment where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (MonadIO)

newtype Reward = Reward Double
  deriving (Show, Num)

class (Monad m) => EnvironmentMonad m where
  type Observation m :: *
  type Action m :: *
  type EnvironmentState m :: *
  currentObservation :: m (Observation m)
  possibleActions :: Observation m -> m [Action m]
  resetEnv :: m (Observation m)
  stepEnv :: (Action m) -> m (Observation m, Reward, Bool)
  runEnv :: (EnvironmentState m) -> m a -> IO a

class (MonadIO m, EnvironmentMonad m) => RenderableEnvironment m where
  renderEnv :: m ()

gameLoop :: (EnvironmentMonad m) => m (Action m) -> m (Observation m, Reward)
gameLoop chooseAction = do
  newAction <- chooseAction
  (newObs, reward, done) <- stepEnv newAction
  if done
    then return (newObs, reward)
    else gameLoop chooseAction

gameRenderLoop :: (RenderableEnvironment m) => m (Action m) -> m (Observation m, Reward)
gameRenderLoop chooseAction = do
  renderEnv
  newAction <- chooseAction
  (newObs, reward, done) <- stepEnv newAction
  if done
    then renderEnv >> return (newObs, reward)
    else gameRenderLoop chooseAction
