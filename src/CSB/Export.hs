module CSB.Export
  ( GameInfo(..)
  )
where

import           CSB.Game
import           CSB.Spec
import           Data.Aeson
import qualified Data.Text                     as Text
import           Data.Vec2

-- | Game specification and simulation result
data GameInfo = GameInfo { _spec :: GameSpec, _simResult :: SimResult }

instance ToJSON GameInfo where
  toJSON GameInfo { _spec = spec, _simResult = simResult } =
    object [Text.pack "spec" .= spec, Text.pack "simResult" .= simResult]

instance ToJSON GameSpec where
  toJSON GameSpec { _laps = laps, _checkpoints = checkpoints } = object
    [ Text.pack "laps" .= laps
    , Text.pack "checkpoints" .= (vec2dToJSON <$> checkpoints)
    ]

instance ToJSON GameState where
  toJSON GameState { _playerStates = Vec2 p1 p2 } =
    object [Text.pack "playerStates" .= [p1, p2]]

instance ToJSON PlayerState where
  toJSON PlayerState { _podStates = Vec2 p1 p2, _timeout = timeout } =
    object [Text.pack "podStates" .= [p1, p2], Text.pack "timeout" .= timeout]

instance ToJSON PodState where
  toJSON PodState { _position = r, _speed = v, _angle = theta, _nextcheckpointid = i, _lap = l, _shieldState = s, _boostAvail = boostAvail }
    = object
      [ Text.pack "position" .= vec2dToJSON r
      , Text.pack "speed" .= vec2dToJSON v
      , Text.pack "angle" .= theta
      , Text.pack "nextcheckpointid" .= i
      , Text.pack "lap" .= l
      , Text.pack "shieldState" .= s
      , Text.pack "boostAvail" .= boostAvail
      ]

instance ToJSON SimResult where
  toJSON SimResult { _initState = initState, _history = history, _outcome = outcome }
    = object
      [ Text.pack "history" .= (initState : (snd <$> history))
      , Text.pack "outcome" .= outcome
      ]

instance ToJSON Outcome where
  toJSON (Win which) =
    object [Text.pack "type" .= "win", Text.pack "which" .= fromEnum which]
  toJSON (Timeout which) =
    object [Text.pack "type" .= "timeout", Text.pack "which" .= fromEnum which]

vec2dToJSON :: Vec2d -> Value
vec2dToJSON (Vec2 x y) = object [Text.pack "x" .= x, Text.pack "y" .= y]
