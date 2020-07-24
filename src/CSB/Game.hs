module CSB.Game
  ( GameState(..)
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
  , SimResult(..)
  , Outcome(..)
  , Player
  , newGame
  , checkEndgame
  , simulateTurn
  , simulateEndToEnd
  , winner
  )
where

import           CSB.Game.Internal.Sim
import           CSB.Game.Internal.Type
import           CSB.Game.Internal.Util
import           CSB.Param
import           CSB.Spec
import           Data.Function
import           Data.Vec2
import           Data.Vector                    ( Vector
                                                , (!)
                                                )
import qualified Data.Vector                   as Vector

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

  r11 = fromIntegral . round <$> perpn (initPodDist / 2) c0 c1
  r21 = fromIntegral . round <$> perpn initPodDist r11 c1

  r12 = fromIntegral . round <$> perp (initPodDist / 2) c0 c1
  r22 = fromIntegral . round <$> perp initPodDist r12 c1

  perpn d v0 v1 = (d `scalarMul` rotaten90 (normalize (v1 - v0))) + v0
  perp d v0 v1 = (d `scalarMul` rotate90 (normalize (v1 - v0))) + v0

-- | Create a new player state at game start.
newPlayerState :: Vec2d -> Vec2d -> PlayerState
newPlayerState initPosition1 initPosition2 = PlayerState
  { _podStates = Vec2 (newPodState initPosition1) (newPodState initPosition2)
  , _timeout   = 100
  }

-- | Create a new pod state at game start.
newPodState :: Vec2d -> PodState
newPodState initPosition = PodState { _position         = initPosition
                                    , _speed            = 0
                                    , _angle            = 0
                                    , _nextcheckpointid = 1
                                    , _lap              = 0
                                    , _shieldState      = 0
                                    , _boostAvail       = True
                                    }

-- * Game Simulation

-- | Check whether or not a game has ended.
checkEndgame :: GameSpec -> GameState -> Maybe Outcome
checkEndgame GameSpec { _laps = laps } state
  | Just which <- whichPlayer (anyPod (\PodState { _lap = l } -> l == laps))
                              state
  = Just (Win which)
  | Just which <- whichPlayer (\PlayerState { _timeout = t } -> t == 0) state
  = Just (Timeout which)
  | otherwise
  = Nothing

-- | Simulate one turn of a game.
simulateTurn :: GameSpec -> TurnOutput -> TurnOutput -> GameState -> GameState
simulateTurn GameSpec { _laps = laps, _checkpoints = checkpoints } o1 o2 state
  = state
    & simulateRotation o1 o2
    & simulateAcceleration o1 o2
    & simulateMovement checkpoints
    & simulateFriction
    & simulateFinal
    & simulateUpdateStarted

-- | End-to-end simulation.
simulateEndToEnd :: (Monad m) => GameSpec -> Player m -> Player m -> m SimResult
simulateEndToEnd spec p1 p2 = simulateStep initState
 where
  initState = newGame spec

  simulateStep state = case checkEndgame spec state of
    Just outcome -> return SimResult { _initState = initState
                                     , _history   = []
                                     , _outcome   = outcome
                                     }
    Nothing -> do
      o1 <- p1 spec state
      o2 <- p2 spec (swapPlayer state)

      let state' = simulateTurn spec o1 o2 state

      SimResult { _history = history, _outcome = outcome } <- simulateStep
        state'
      return SimResult { _initState = initState
                       , _history   = (Vec2 o1 o2, state') : history
                       , _outcome   = outcome
                       }

-- | Determine the winner from an outcome.
winner :: Outcome -> PlayerIx
winner (Win     p) = p
winner (Timeout p) = theOtherPlayer p
 where
  theOtherPlayer Player1 = Player2
  theOtherPlayer Player2 = Player1
