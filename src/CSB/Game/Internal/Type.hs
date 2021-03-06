module CSB.Game.Internal.Type
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
  )
where

import           CSB.Internal.JSON
import           CSB.Spec
import           Data.Aeson
import           Data.Vec2
import           Data.Vector                    ( Vector )
import           GHC.Generics

-- * State Information

-- | State for a Coders Strike Back game.
data GameState = GameState { _playerStates :: Vec2 PlayerState
                           , _started      :: Bool
                           } deriving (Eq, Show, Read, Generic)

instance FromJSON GameState where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON GameState where
  toJSON     = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

-- | State for a player.
data PlayerState = PlayerState { _podStates  :: Vec2 PodState
                               , _timeout    :: Int
                               } deriving (Eq, Show, Read, Generic)

instance FromJSON PlayerState where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON PlayerState where
  toJSON     = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

-- | State for a pod.
data PodState = PodState { _position         :: Vec2d
                         , _speed            :: Vec2d
                         , _angle            :: Double
                         , _nextCheckPointId :: Int
                         , _lap              :: Int
                         , _shieldState      :: Int
                         , _boostAvail       :: Bool
                         } deriving (Eq, Show, Read, Generic)

instance FromJSON PodState where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON PodState where
  toJSON     = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

-- ** Indexing.

-- | The type used to index an individual PlayerState in a GameState.
data PlayerIx = Player1 | Player2
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic)

instance FromJSON PlayerIx where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON PlayerIx where
  toJSON     = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

-- | Get a PlayerState by its index in a GameState.
getPlayer :: PlayerIx -> GameState -> PlayerState
getPlayer Player1 GameState { _playerStates = Vec2 p1 _ } = p1
getPlayer Player2 GameState { _playerStates = Vec2 _ p2 } = p2

-- | Modify a PlayerState indexed by a given index in a GameState.
modifyPlayer
  :: PlayerIx -> (PlayerState -> PlayerState) -> GameState -> GameState
modifyPlayer Player1 f state@GameState { _playerStates = Vec2 p1 p2 } =
  state { _playerStates = Vec2 (f p1) p2 }
modifyPlayer Player2 f state@GameState { _playerStates = Vec2 p1 p2 } =
  state { _playerStates = Vec2 p1 (f p2) }

-- | Write a PlayerState indexed by a given index in a GameState.
writePlayer :: PlayerIx -> PlayerState -> GameState -> GameState
writePlayer i value = modifyPlayer i (const value)

-- | The type used to index an individual PodState in a PlayerState.
data PodIx = Pod1 | Pod2 deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- | Get a PodState by its index in a PlayerState.
getPod :: PodIx -> PlayerState -> PodState
getPod Pod1 PlayerState { _podStates = Vec2 p1 _ } = p1
getPod Pod2 PlayerState { _podStates = Vec2 _ p2 } = p2

-- | Modify a PodState indexed by a given index in a PlayerState.
modifyPod :: PodIx -> (PodState -> PodState) -> PlayerState -> PlayerState
modifyPod Pod1 f state@PlayerState { _podStates = Vec2 p1 p2 } =
  state { _podStates = Vec2 (f p1) p2 }
modifyPod Pod2 f state@PlayerState { _podStates = Vec2 p1 p2 } =
  state { _podStates = Vec2 p1 (f p2) }

-- | Write a PodState indexed by a given index in a PlayerState.
writePod :: PodIx -> PodState -> PlayerState -> PlayerState
writePod i value = modifyPod i (const value)

-- | Get a PodState by its index in a GameState.
getPodInGame :: (PlayerIx, PodIx) -> GameState -> PodState
getPodInGame (i, j) = getPod j . getPlayer i

-- | Modify a PodState indexed by a given index in a GameState.
modifyPodInGame
  :: (PlayerIx, PodIx) -> (PodState -> PodState) -> GameState -> GameState
modifyPodInGame (i, j) f = modifyPlayer i (modifyPod j f)

-- | Write a PodState indexed by a given index in a GameState.
writePodInGame :: (PlayerIx, PodIx) -> PodState -> GameState -> GameState
writePodInGame i value = modifyPodInGame i (const value)

-- * Player Outputs

-- | Turn output.
type TurnOutput = Vec2 Instruction

-- | Instruction to control a pod.
data Instruction = Instruction { _target :: Vec2d, _thrust :: Thrust }
  deriving (Eq, Show, Read, Generic)

instance FromJSON Instruction where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON Instruction where
  toJSON     = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

-- | Thrust of a pod.
data Thrust = Thrust Int | Shield | Boost deriving (Eq, Show, Read, Generic)

instance FromJSON Thrust where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON Thrust where
  toJSON     = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

-- * Simulation Result

-- | The complete simulation result of a game.
data SimResult = SimResult { _initState :: GameState
                           , _history :: [(Vec2 TurnOutput, GameState)]
                           , _outcome :: Outcome
                           } deriving (Eq, Show, Read, Generic)

instance FromJSON SimResult where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON SimResult where
  toJSON     = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

-- | The outcome of a game, i.e., who wins or who times out.
data Outcome = Win PlayerIx | Timeout PlayerIx
  deriving (Eq, Show, Read, Generic)

instance FromJSON Outcome where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON Outcome where
  toJSON     = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts

-- * Player

-- | A player. I.e., the unit that controls the 2 pods on its team.
type Player m = GameSpec -> GameState -> m TurnOutput

-- | Game specification and simulation result
data GameInfo = GameInfo { _spec :: GameSpec, _simResult :: SimResult }
  deriving (Eq, Show, Read, Generic)

instance FromJSON GameInfo where
  parseJSON = genericParseJSON jsonOpts

instance ToJSON GameInfo where
  toJSON     = genericToJSON jsonOpts
  toEncoding = genericToEncoding jsonOpts
