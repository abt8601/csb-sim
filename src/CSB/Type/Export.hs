module CSB.Type.Export where

import           Data.Aeson
import qualified Data.Text                     as Text

import           CSB.Type
import           Data.Vec2

instance ToJSON GameState where
  toJSON GameState { _playerStates = Vec2 p1 p2 } =
    object [Text.pack "playerStates" .= [p1, p2]]

instance ToJSON PlayerState where
  toJSON PlayerState { _podStates = Vec2 p1 p2, _boostAvail = boostAvail, _timeout = timeout }
    = object
      [ Text.pack "podStates" .= [p1, p2]
      , Text.pack "boostAvail" .= boostAvail
      , Text.pack "timeout" .= timeout
      ]

instance ToJSON PodState where
  toJSON PodState { _position = r, _speed = v, _angle = theta, _nextcheckpointid = i, _lap = l, _shieldState = s }
    = object
      [ Text.pack "position" .= vec2dToJSON r
      , Text.pack "speed" .= vec2dToJSON v
      , Text.pack "angle" .= theta
      , Text.pack "nextcheckpointid" .= i
      , Text.pack "lap" .= l
      , Text.pack "shieldState" .= s
      ]
   where
    vec2dToJSON (Vec2 x y) = object [Text.pack "x" .= x, Text.pack "y" .= y]

instance ToJSON SimResult where
  toJSON SimResult { _history = history, _outcome = outcome } =
    object [Text.pack "history" .= history, Text.pack "outcome" .= outcome]

instance ToJSON Outcome where
  toJSON (Win which) =
    object [Text.pack "type" .= "win", Text.pack "which" .= fromEnum which]
  toJSON (Timeout which) =
    object [Text.pack "type" .= "timeout", Text.pack "which" .= fromEnum which]
