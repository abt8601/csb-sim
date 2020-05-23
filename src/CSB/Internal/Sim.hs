module CSB.Internal.Sim
  ( simulateRotation
  , simulateAcceleration
  , simulateMovement
  , simulateFriction
  , simulateFinal
  , simulateUpdateStarted
  )
where

import           Data.Function
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector

import           CSB.Internal.CSBTypeUtil
import           CSB.Internal.Param
import           CSB.Internal.Physics
import           CSB.Internal.Util
import           CSB.Type
import           Data.Vec2

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
simulateAcceleration = perPlayerI simulateAccelerationPerPlayer

simulateAccelerationPerPlayer :: TurnOutput -> PlayerState -> PlayerState
simulateAccelerationPerPlayer (Vec2 i1 i2) state@PlayerState { _podStates = Vec2 s1 s2, _boostAvail = boostAvail }
  = state { _podStates = Vec2 s1' s2', _boostAvail = boostAvail' }
 where
  (s1', boostAvailInt) = simulateAccelerationPerPod boostAvail i1 s1
  (s2', boostAvail'  ) = simulateAccelerationPerPod boostAvail i2 s2

simulateAccelerationPerPod
  :: Bool -> Instruction -> PodState -> (PodState, Bool)
simulateAccelerationPerPod boostAvail Instruction { _thrust = thr } state@PodState { _speed = v, _angle = theta, _shieldState = s }
  = (state { _speed = v + a, _shieldState = s' }, boostAvail')
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
simulateMovement checkpoints = simulateMovementTime checkpoints 0 . perPlayer
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
updateCheckpointPod nCheckpoints state@PodState { _nextcheckpointid = i, _lap = l }
  = state { _nextcheckpointid = i', _lap = l' }
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
