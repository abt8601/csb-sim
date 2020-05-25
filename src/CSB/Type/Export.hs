module CSB.Type.Export where

import           Data.Aeson
import qualified Data.Text                     as Text
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as Vector

import           CSB.Type
import           Data.Vec2

instance ToJSON GameState where
  toJSON GameState { _playerStates = Vec2 p1 p2 } = object
    [Text.pack "playerStates" .= Array (Vector.fromList (toJSON <$> [p1, p2]))]

instance ToJSON PlayerState where
  toJSON PlayerState { _podStates = Vec2 p1 p2, _boostAvail = boostAvail, _timeout = timeout }
    = object
      [ Text.pack "podStates" .= Array (Vector.fromList (toJSON <$> [p1, p2]))
      , Text.pack "boostAvail" .= toJSON boostAvail
      , Text.pack "timeout" .= toJSON timeout
      ]

instance ToJSON PodState where
  toJSON PodState { _position = r, _speed = v, _angle = theta, _nextcheckpointid = i, _lap = l, _shieldState = s }
    = object
      [ Text.pack "position" .= vec2dToJSON r
      , Text.pack "speed" .= vec2dToJSON v
      , Text.pack "angle" .= toJSON theta
      , Text.pack "nextcheckpointid" .= toJSON i
      , Text.pack "lap" .= toJSON l
      , Text.pack "shieldState" .= toJSON s
      ]
   where
    vec2dToJSON (Vec2 x y) =
      object [Text.pack "x" .= toJSON x, Text.pack "y" .= y]

instance ToJSON SimResult where
  toJSON SimResult { _history = history, _outcome = outcome } =
    object [Text.pack "history" .= history, Text.pack "outcome" .= outcome]

instance ToJSON Outcome where
  toJSON (Win which) =
    object [Text.pack "type" .= "win", Text.pack "which" .= fromEnum which]
  toJSON (Timeout which) =
    object [Text.pack "type" .= "timeout", Text.pack "which" .= fromEnum which]
