module CSB.Game
  ( GameSpec(..)
  , GameState(..)
  , PlayerState(..)
  , PodState(..)
  , newGame
  )
where

import           Data.Vector                    ( Vector
                                                , (!)
                                                )
import qualified Data.Vector                   as Vector

import           CSB.Param
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

-- * Game Creation

-- | Create a new Coders Strike Back game.
newGame :: GameSpec -> GameState
newGame GameSpec { _checkpoints = checkpoints } = GameState
  { _playerStates = Vec2 (newPlayerState r11 r12) (newPlayerState r21 r22)
  , _started      = False
  }
 where
  c0  = checkpoints ! 0
  c1  = checkpoints ! 1

  r11 = perpn (initPodDist / 2) c0 c1
  r21 = perpn initPodDist r11 c1

  r12 = perp (initPodDist / 2) c0 c1
  r22 = perp initPodDist r12 c1

  perpn d v0 v1 = (d `scalarMul` rotaten90 (normalize (v1 - v0))) + v0
  perp d v0 v1 = (d `scalarMul` rotate90 (normalize (v1 - v0))) + v0

-- | Create a new player state at game start.
newPlayerState :: Vec2d -> Vec2d -> PlayerState
newPlayerState initPosition1 initPosition2 = PlayerState
  { _podStates  = Vec2 (newPodState initPosition1) (newPodState initPosition2)
  , _boostAvail = True
  , _timeout    = 100
  }

-- | Create a new pod state at game start.
newPodState :: Vec2d -> PodState
newPodState initPosition = PodState { _position         = initPosition
                                    , _speed            = 0
                                    , _angle            = 0
                                    , _nextcheckpointid = 1
                                    , _lap              = 0
                                    , _shieldState      = 0
                                    }
