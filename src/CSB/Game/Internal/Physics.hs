module CSB.Game.Internal.Physics
  ( movePodsInertial
  , CollisionType(..)
  , detectAllCollisions
  , bounceI
  )
where

import           Control.Applicative
import           CSB.Game.Internal.Type
import           CSB.Game.Internal.Util
import           CSB.Param
import           Data.List
import           Data.Vec2
import           Data.Vector                    ( Vector
                                                , (!)
                                                )
import qualified Data.Vector                   as Vector

-- * Inertial

-- | Move pods according to its inertia.
movePodsInertial :: Double -> GameState -> GameState
movePodsInertial time = perPod (movePodInertial time)

-- | Move a pod according to its inertia.
movePodInertial :: Double -> PodState -> PodState
movePodInertial time state@PodState { _position = r, _speed = v } =
  state { _position = r + time `scalarMul` v }

-- * Collision

-- | Types of collision.
data CollisionType = PodPod (PlayerIx, PodIx) (PlayerIx, PodIx)
                   | PodCheckpoint (PlayerIx, PodIx)
                   deriving (Eq, Show, Read)

-- | Pod indices.
podIxs = liftA2 (,) [Player1, Player2] [Pod1, Pod2]

-- | Distinct pairs of pod indices.
podIxPairs :: [((PlayerIx, PodIx), (PlayerIx, PodIx))]
podIxPairs = gen podIxs
 where
  gen []       = []
  gen (i : is) = ((,) i <$> is) ++ gen is

-- | Find the time it takes for a point travelling at constant speed to get to
-- | some distance away from the origin. Returns infinity if it never happens.
timeToCollision :: Vec2d -> Vec2d -> Double -> Double
timeToCollision r v d = if collision then time else 1 / 0
 where
  pvr       = proj v r
  compl     = r - pvr
  vs        = sqrt (d ^ 2 - sqnorm compl) `scalarMul` normalize pvr
  time      = ((vs - pvr) `dot` v) / v `dot` v

  collision = sqnorm compl < d ^ 2 && time > eps

  eps       = 1e-6

-- | Find the time it takes for 2 pods travelling at constant speed to collide
-- | with each other.
timeToCollisionPods :: PodState -> PodState -> Double
timeToCollisionPods PodState { _position = r1, _speed = v1 } PodState { _position = r2, _speed = v2 }
  = timeToCollision (r2 - r1) (v2 - v1) (2 * podForceFieldRadius)

-- | Find the time it takes for 2 indexed pods travelling at constant speed to
-- | collide with each other.
timeToCollisionPodsI
  :: (PlayerIx, PodIx) -> (PlayerIx, PodIx) -> GameState -> Double
timeToCollisionPodsI i j state =
  timeToCollisionPods (getPodInGame i state) (getPodInGame j state)

-- | Find the time it takes for an indexed pod travelling at constant speed and
-- | its next checkpoint to collide with each other.
timeToCollisionPodCheckpointI
  :: Vector Vec2d -> (PlayerIx, PodIx) -> GameState -> Double
timeToCollisionPodCheckpointI checkpoints i state = timeToCollision
  (r - checkpoints ! j)
  v
  checkpointRadius
 where
  PodState { _position = r, _speed = v, _nextCheckPointId = j } =
    getPodInGame i state

-- | Detect all possible collisions.
detectAllCollisions :: Vector Vec2d -> GameState -> [(CollisionType, Double)]
detectAllCollisions checkpoints state =
  sortOn snd $ podPodCollisions ++ podCheckpointCollisions
 where
  podPodCollisions =
    (\(i, j) -> (PodPod i j, timeToCollisionPodsI i j state)) <$> podIxPairs
  podCheckpointCollisions =
    (\i -> (PodCheckpoint i, timeToCollisionPodCheckpointI checkpoints i state))
      <$> podIxs

-- | Bounce two pods away.
bounce :: PodState -> PodState -> (PodState, PodState)
bounce p1@PodState { _position = r1, _speed = v1, _shieldState = s1 } p2@PodState { _position = r2, _speed = v2, _shieldState = s2 }
  = (p1 { _speed = v1' }, p2 { _speed = v2' })
 where
  m1 = if s1 == 3 then 10 else 1
  m2 = if s2 == 3 then 10 else 1

  jp = (2 * m1 * m2 / (m1 + m2)) `scalarMul` proj (r1 - r2) (v1 - v2)
  j  = if norm jp < collisionMinImpulse
    then collisionMinImpulse `scalarMul` normalize jp
    else jp

  v1' = v1 - j `scalarDiv` m1
  v2' = v2 + j `scalarDiv` m2


-- | Bounce two indexed pods away.
bounceI :: (PlayerIx, PodIx) -> (PlayerIx, PodIx) -> GameState -> GameState
bounceI i j state = writePodInGame i p1 . writePodInGame j p2 $ state
  where (p1, p2) = bounce (getPodInGame i state) (getPodInGame j state)
