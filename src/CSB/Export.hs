module CSB.Export
  ( GameInfo(..)
  )
where

import           CSB.Game
import           CSB.Spec
import           Data.Aeson
import           Data.Vec2

-- | Game specification and simulation result
data GameInfo = GameInfo { _spec :: GameSpec, _simResult :: SimResult }

instance ToJSON GameInfo where
  toJSON GameInfo { _spec = spec, _simResult = simResult } =
    object ["spec" .= spec, "simResult" .= simResult]

instance ToJSON GameSpec where
  toJSON GameSpec { _laps = laps, _checkpoints = checkpoints } =
    object ["laps" .= laps, "checkpoints" .= (vec2dToJSON <$> checkpoints)]

instance ToJSON GameState where
  toJSON GameState { _playerStates = Vec2 p1 p2 } =
    object ["playerStates" .= [p1, p2]]

instance ToJSON PlayerState where
  toJSON PlayerState { _podStates = Vec2 p1 p2, _timeout = timeout } =
    object ["podStates" .= [p1, p2], "timeout" .= timeout]

instance ToJSON PodState where
  toJSON PodState { _position = r, _speed = v, _angle = theta, _nextcheckpointid = i, _lap = l, _shieldState = s, _boostAvail = boostAvail }
    = object
      [ "position" .= vec2dToJSON r
      , "speed" .= vec2dToJSON v
      , "angle" .= theta
      , "nextcheckpointid" .= i
      , "lap" .= l
      , "shieldState" .= s
      , "boostAvail" .= boostAvail
      ]

instance ToJSON SimResult where
  toJSON SimResult { _initState = initState, _history = history, _outcome = outcome }
    = object
      ["history" .= (initState : (snd <$> history)), "outcome" .= outcome]

instance ToJSON Outcome where
  toJSON (Win which) =
    object ["type" .= ("win" :: String), "which" .= fromEnum which]
  toJSON (Timeout which) =
    object ["type" .= ("timeout" :: String), "which" .= fromEnum which]

vec2dToJSON :: Vec2d -> Value
vec2dToJSON (Vec2 x y) = object ["x" .= x, "y" .= y]
