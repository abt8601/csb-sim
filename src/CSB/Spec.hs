module CSB.Spec
  ( GameSpec(..)
  , randomSpec
  )
where

import           Control.Monad.State
import           CSB.Internal.Param
import           Data.Functor.Identity
import           Data.Vec2
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector
import           System.Random

-- * Game Specification

-- | Specification for a Coders Strike Back game.
data GameSpec = GameSpec { _laps        :: Int
                         , _checkpoints :: Vector Vec2d
                         } deriving (Eq, Show, Read)

-- | Generate a random specification.
randomSpec :: (RandomGen g) => Int -> g -> (GameSpec, g)
randomSpec laps = runState $ do
  nCheckpoints <- state (randomR (3, 8))
  checkpoints  <- Vector.fromList <$> genCheckpoints nCheckpoints
  return GameSpec { _laps = laps, _checkpoints = checkpoints }
 where
  genCheckpoints :: (RandomGen g) => Int -> State g [Vec2d]
  genCheckpoints 0 = return []
  genCheckpoints n = do
    others <- genCheckpoints (pred n)
    this   <- fix $ \rec -> do
      candidate <- randomCheckpoint
      if hasOverlap candidate others then rec else return candidate
    return $ this : others

  randomCheckpoint :: (RandomGen g) => State g Vec2d
  randomCheckpoint =
    Vec2 <$> state (randomR (1000, 15000)) <*> state (randomR (1000, 8000))

  hasOverlap checkpoint = any ((< (2 * checkpointRadius)) . dist checkpoint)
