module CSB.Spec
  ( GameSpec(..)
  )
where

import           Data.Vec2
import           Data.Vector                    ( Vector )

-- * Game Specification

-- | Specification for a Coders Strike Back game.
data GameSpec = GameSpec { _laps        :: Int
                         , _checkpoints :: Vector Vec2d
                         } deriving (Eq, Show, Read)
