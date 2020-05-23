module CSB.Type
  ( GameSpec(..)
  , GameState(..)
  , PlayerState(..)
  , PodState(..)
  )
where

import           Data.Vector                    ( Vector )

import           Data.Vec2

-- * Game Specification

-- | Specification for a Coders Strike Back game.
data GameSpec = GameSpec { _laps        :: Int
                         , _checkpoints :: Vector Vec2d
                         } deriving (Eq, Show, Read)

-- * State Information

-- | State for a Coders Strike Back game.
data GameState = GameState { _playerStates :: Vec2 PlayerState
                           , _started      :: Bool
                           } deriving (Eq, Show, Read)

-- | State for a player.
data PlayerState = PlayerState { _podStates  :: Vec2 PodState
                               , _boostAvail :: Bool
                               , _timeout    :: Int
                               } deriving (Eq, Show, Read)

-- | State for a pod.
data PodState = PodState { _position         :: Vec2d
                         , _speed            :: Vec2d
                         , _angle            :: Double
                         , _nextcheckpointid :: Int
                         , _lap              :: Int
                         , _shieldState      :: Int
                         } deriving (Eq, Show, Read)
