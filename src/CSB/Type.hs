module CSB.Type
  ( GameSpec(..)
  , GameState(..)
  , PlayerState(..)
  , PodState(..)
  , PlayerIx(..)
  , getPlayer
  , modifyPlayer
  , writePlayer
  , PodIx(..)
  , getPod
  , modifyPod
  , writePod
  , getPodInGame
  , modifyPodInGame
  , writePodInGame
  , TurnOutput(..)
  , Instruction(..)
  , Thrust(..)
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

-- ** Indexing.

-- | The type used to index an individual PlayerState in a GameState.
data PlayerIx = Player1 | Player2 deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | Get a PlayerState by its index in a GameState.
getPlayer :: PlayerIx -> GameState -> PlayerState
getPlayer Player1 GameState { _playerStates = Vec2 p1 _ } = p1
getPlayer Player2 GameState { _playerStates = Vec2 _ p2 } = p2

-- | Modify a PlayerState indexed by a given index in a GameState.
modifyPlayer
  :: PlayerIx -> (PlayerState -> PlayerState) -> GameState -> GameState
modifyPlayer Player1 f state@GameState { _playerStates = Vec2 p1 p2 } =
  state { _playerStates = Vec2 (f p1) p2 }
modifyPlayer Player2 f state@GameState { _playerStates = Vec2 p1 p2 } =
  state { _playerStates = Vec2 p1 (f p2) }

-- | Write a PlayerState indexed by a given index in a GameState.
writePlayer :: PlayerIx -> PlayerState -> GameState -> GameState
writePlayer i value = modifyPlayer i (const value)

-- | The type used to index an individual PodState in a PlayerState.
data PodIx = Pod1 | Pod2 deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | Get a PodState by its index in a PlayerState.
getPod :: PodIx -> PlayerState -> PodState
getPod Pod1 PlayerState { _podStates = Vec2 p1 _ } = p1
getPod Pod2 PlayerState { _podStates = Vec2 _ p2 } = p2

-- | Modify a PodState indexed by a given index in a PlayerState.
modifyPod :: PodIx -> (PodState -> PodState) -> PlayerState -> PlayerState
modifyPod Pod1 f state@PlayerState { _podStates = Vec2 p1 p2 } =
  state { _podStates = Vec2 (f p1) p2 }
modifyPod Pod2 f state@PlayerState { _podStates = Vec2 p1 p2 } =
  state { _podStates = Vec2 p1 (f p2) }

-- | Write a PodState indexed by a given index in a PlayerState.
writePod :: PodIx -> PodState -> PlayerState -> PlayerState
writePod i value = modifyPod i (const value)

-- | Get a PodState by its index in a GameState.
getPodInGame :: (PlayerIx, PodIx) -> GameState -> PodState
getPodInGame (i, j) = getPod j . getPlayer i

-- | Modify a PodState indexed by a given index in a GameState.
modifyPodInGame
  :: (PlayerIx, PodIx) -> (PodState -> PodState) -> GameState -> GameState
modifyPodInGame (i, j) f = modifyPlayer i (modifyPod j f)

-- | Write a PodState indexed by a given index in a GameState.
writePodInGame :: (PlayerIx, PodIx) -> PodState -> GameState -> GameState
writePodInGame i value = modifyPodInGame i (const value)

-- * Player Outputs

-- | Turn output.
type TurnOutput = Vec2 Instruction

-- | Instruction to control a pod.
data Instruction = Instruction { _target :: Vec2d, _thrust :: Thrust }
  deriving (Eq, Show, Read)

-- | Thrust of a pod.
data Thrust = Thrust Int | Shield | Boost deriving (Eq, Show, Read)
