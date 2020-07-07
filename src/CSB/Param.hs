module CSB.Param
  ( initTimeout
  , initPodDist
  , maxTurnAngle
  , podForceFieldRadius
  , checkpointRadius
  , collisionMinImpulse
  )
where

-- | The initial timeout of a player.
initTimeout :: Int
initTimeout = 100

-- | The distance between two pods at game start.
initPodDist :: Double
initPodDist = 1000

-- | Maximum turn angle of a pod per turn.
maxTurnAngle :: Double
maxTurnAngle = pi / 10 -- 18 degrees.

-- | The radius of the force field around a pod.
podForceFieldRadius :: Double
podForceFieldRadius = 400

-- | The radius of the checkpoint.
checkpointRadius :: Double
checkpointRadius = 600

-- | The minimum impulse of collision.
collisionMinImpulse :: Double
collisionMinImpulse = 120
