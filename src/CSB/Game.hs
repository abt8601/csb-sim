module CSB.Game
  ( newGame
  , SimulationResult(..)
  , EndGameResult(..)
  , simulateTurn
  )
where

import           Data.Function
import           Data.Vector                    ( Vector
                                                , (!)
                                                )
import qualified Data.Vector                   as Vector

import           CSB.Internal.CSBTypeUtil
import           CSB.Internal.Param
import           CSB.Internal.Sim
import           CSB.Type
import           Data.Vec2

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

-- * Game Simulation

-- | The result of simulation.
data SimulationResult = Ongoing GameState | Ended EndGameResult
  deriving (Eq, Show, Read)

-- | The result after a game ends.
data EndGameResult = Win PlayerIx | Timeout PlayerIx deriving (Eq, Show, Read)

-- | Simulate one turn of a game.
simulateTurn
  :: GameSpec -> TurnOutput -> TurnOutput -> GameState -> SimulationResult
simulateTurn GameSpec { _laps = laps, _checkpoints = checkpoints } o1 o2 state
  | Just which <- whichPlayer (anyPod (\PodState { _lap = l } -> l == laps))
                              state
  = Ended (Win which)
  | Just which <- whichPlayer (\PlayerState { _timeout = t } -> t == 0) state
  = Ended (Timeout which)
  | otherwise
  = Ongoing
    $ state
    & simulateRotation o1 o2
    & simulateAcceleration o1 o2
    & simulateMovement checkpoints
    & simulateFriction
    & simulateFinal
    & simulateUpdateStarted
