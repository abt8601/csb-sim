module CSB.Game.Internal.Sim
  ( simulateRotation
  , simulateAcceleration
  , simulateMovement
  , simulateFriction
  , simulateFinal
  , simulateUpdateStarted
  )
where

import           CSB.Game.Internal.Physics
import           CSB.Game.Internal.Type
import           CSB.Game.Internal.Util
import           CSB.Internal.Util
import           CSB.Param
import           Data.Function
import           Data.Vec2
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector

-- * Simulation Pipeline

simulateRotation :: TurnOutput -> TurnOutput -> GameState -> GameState
simulateRotation o1 o2 state@GameState { _started = started } =
  perPodI (simulateRotationPerPod started) o1 o2 state

simulateRotationPerPod :: Bool -> Instruction -> PodState -> PodState
simulateRotationPerPod started Instruction { _target = target } state@PodState { _position = r, _angle = theta }
  = state { _angle = theta' }
 where
  deltaThetaReq = normalizeAngle $ arg (target - r) - theta
  deltaTheta    = if started
    then clamp (-maxTurnAngle) maxTurnAngle deltaThetaReq
    else deltaThetaReq
  theta' = theta + deltaTheta

simulateAcceleration :: TurnOutput -> TurnOutput -> GameState -> GameState
simulateAcceleration = perPodI simulateAccelerationPerPod

simulateAccelerationPerPod :: Instruction -> PodState -> PodState
simulateAccelerationPerPod Instruction { _thrust = thr } state@PodState { _speed = v, _angle = theta, _shieldState = s, _boostAvail = boostAvail }
  = state { _speed = v + a, _shieldState = s', _boostAvail = boostAvail' }
 where
  acceleratable                   = s == 0
  (normA, useShield, boostAvail') = case thr of
    Thrust n ->
      (if acceleratable then fromIntegral n else 0, False, boostAvail)
    Shield -> (0, True, boostAvail)
    Boost  -> if acceleratable
      then (if boostAvail then 650 else 100, False, False)
      else (0, False, boostAvail)
  a  = normA `scalarMul` rotate theta (Vec2 1 0)
  s' = if useShield then 3 else max 0 (pred s)

simulateMovement :: Vector Vec2d -> GameState -> GameState
simulateMovement checkpoints = simulateMovementTime checkpoints 1 . perPlayer
  (\state@PlayerState { _timeout = t } -> state { _timeout = pred t })

simulateMovementTime :: Vector Vec2d -> Double -> GameState -> GameState
simulateMovementTime checkpoints time state = handleCollisions
  (detectAllCollisions checkpoints state)
  state
 where
  handleCollisions [] state                       = movePodsInertial time state
  handleCollisions ((_, t) : _) state | t >= time = movePodsInertial time state
  handleCollisions ((PodPod i j, t) : _) state =
    state & movePodsInertial t & bounceI i j & simulateMovementTime
      checkpoints
      (time - t)
  handleCollisions ((PodCheckpoint i, _) : cs) state =
    state & updateCheckpoint (Vector.length checkpoints) i & handleCollisions cs

updateCheckpoint :: Int -> (PlayerIx, PodIx) -> GameState -> GameState
updateCheckpoint nCheckpoints (i, j) =
  modifyPlayer i (updateCheckpointPlayer nCheckpoints j)

updateCheckpointPlayer :: Int -> PodIx -> PlayerState -> PlayerState
updateCheckpointPlayer nCheckpoints i state = modifyPod
  i
  (updateCheckpointPod nCheckpoints)
  state { _timeout = initTimeout }

updateCheckpointPod :: Int -> PodState -> PodState
updateCheckpointPod nCheckpoints state@PodState { _nextCheckPointId = i, _lap = l }
  = state { _nextCheckPointId = i', _lap = l' }
 where
  i' = if succ i == nCheckpoints then 0 else succ i
  l' = if i == 0 then succ l else l

simulateFriction :: GameState -> GameState
simulateFriction = perPod simulateFrictionPerPod

simulateFrictionPerPod :: PodState -> PodState
simulateFrictionPerPod state@PodState { _speed = v } =
  state { _speed = 0.85 `scalarMul` v }

simulateFinal :: GameState -> GameState
simulateFinal = perPod simulateFinalPerPod

simulateFinalPerPod :: PodState -> PodState
simulateFinalPerPod state@PodState { _position = r, _speed = v } = state
  { _position = fromIntegral . round <$> r
  , _speed    = fromIntegral . truncate <$> v
  }

simulateUpdateStarted :: GameState -> GameState
simulateUpdateStarted state = state { _started = True }
