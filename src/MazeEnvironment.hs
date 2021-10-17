{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module MazeEnvironment where

import qualified Data.Array as A
import Data.List (groupBy)
import Control.Monad.State
import BFS
import Environment
import Game

data Direction = MoveUp | MoveLeft | MoveDown | MoveRight | MoveNone
  deriving (Show, Enum)

newtype MazeGameM a = MazeGameM (StateT MazeGameState IO a)
  deriving (Functor, Applicative, Monad)

instance (MonadState MazeGameState) MazeGameM where
  get = MazeGameM get
  put env = MazeGameM $ put env

instance MonadIO MazeGameM where
  liftIO action = MazeGameM (lift action)

data MazeGameState = MazeGameState
  { playerLoc :: Location
  , startLoc :: Location
  , endLoc :: Location
  , gameGrid :: Grid
  }

resetGame :: MazeGameM Location
resetGame = do
  current <- get
  put $ current { playerLoc = startLoc current }
  return (startLoc current)

stepGame :: Direction -> MazeGameM (Location, Reward, Bool)
stepGame direction = do
  current <- get
  let currentLoc = playerLoc current
  let nextLoc = findNextLoc direction currentLoc
  let isValid = isValidLoc nextLoc (gameGrid current)
  let finalLoc = if isValid then nextLoc else currentLoc
  put $ current { playerLoc = finalLoc }
  let done = finalLoc == endLoc current
  let reward = if currentLoc /= finalLoc && done
                  then Reward 50.0
                  else if not isValid
                    then Reward (-1.0)
                    else Reward (-0.1)
  return (finalLoc, reward, done)

instance EnvironmentMonad MazeGameM where
  type (Observation MazeGameM) = Location
  type (Action MazeGameM) = Direction
  type (EnvironmentState MazeGameM) = MazeGameState
  currentObservation = MazeGameM (playerLoc <$> get)
  resetEnv = resetGame
  stepEnv = stepGame
  possibleActions _ = return [MoveUp, MoveLeft, MoveDown, MoveRight, MoveNone]
  runEnv env (MazeGameM action) = evalStateT action env

baseEnvironment :: MazeGameState
baseEnvironment = (MazeGameState (0, 0) (0, 0) (2, 2) baseGrid)

instance RenderableEnvironment MazeGameM where
  renderEnv = do
    env <- get
    let rows = groupBy (\((a, _), _) ((b, _), _) -> a == b) (A.assocs (gameGrid env))
    forM_ rows $ \row -> liftIO $ do
      forM_ row $ \(l, _) -> putChar (charForLoc env l)
      putStr "\n"
    liftIO $ putStr "\n"
    where
      charForLoc env loc = if loc == playerLoc env
        then 'o'
        else if loc == endLoc env
          then 'F'
          else if gameGrid env A.! loc == Wall then 'x' else '_'

chooseMoveMaze :: MazeGameM Direction
chooseMoveMaze = do
  env <- get
  let path = bfsSearch (gameGrid env) (playerLoc env) (endLoc env)
  case path of
    [] -> return MoveNone
    (move : _) -> return $ moveDirection (playerLoc env) move

moveDirection :: Location -> Location -> Direction
moveDirection (r, c) nextLoc
  | nextLoc == (r - 1, c) = MoveUp
  | nextLoc == (r, c - 1) = MoveLeft
  | nextLoc == (r + 1, c) = MoveDown
  | nextLoc == (r, c + 1) = MoveRight
  | otherwise = MoveNone

findNextLoc :: Direction -> Location -> Location
findNextLoc MoveUp (r, c) = (r - 1, c)
findNextLoc MoveLeft (r, c) = (r, c - 1)
findNextLoc MoveDown (r, c) = (r + 1, c)
findNextLoc MoveRight (r, c) = (r, c + 1)
findNextLoc MoveNone (r, c) = (r, c)

isValidLoc :: Location -> Grid -> Bool
isValidLoc (r, c) grid =
  r >= 0 &&
  c >= 0 &&
  r <= (fst . snd) (A.bounds grid) &&
  c <= (snd . snd) (A.bounds grid) &&
  grid A.! (r, c) == Empty
