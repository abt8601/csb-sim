module CSB.Internal.CSBTypeUtil
  ( perPlayer
  , perPod
  , perPlayerI
  , perPodI
  , whichPlayer
  , anyPod
  )
where

import           CSB.Type
import           Data.Vec2

-- | Apply a function on each PlayerState of a GameState.
perPlayer :: (PlayerState -> PlayerState) -> GameState -> GameState
perPlayer f state@GameState { _playerStates = Vec2 s1 s2 } =
  state { _playerStates = Vec2 (f s1) (f s2) }

-- | Apply a function on each PodState of a PlayerState.
perPodOfPlayer :: (PodState -> PodState) -> PlayerState -> PlayerState
perPodOfPlayer f state@PlayerState { _podStates = Vec2 s1 s2 } =
  state { _podStates = Vec2 (f s1) (f s2) }

-- | Apply a function on each PodState of a GameState.
perPod :: (PodState -> PodState) -> GameState -> GameState
perPod f = perPlayer (perPodOfPlayer f)

-- | Apply a function on each TurnOutput-PlayerState pair of a GameState.
perPlayerI
  :: (TurnOutput -> PlayerState -> PlayerState)
  -> TurnOutput
  -> TurnOutput
  -> GameState
  -> GameState
perPlayerI f o1 o2 state@GameState { _playerStates = Vec2 s1 s2 } =
  state { _playerStates = Vec2 (f o1 s1) (f o2 s2) }

-- | Apply a function on each Instruction-PodState pair of a PlayerState.
perPodOfPlayerI
  :: (Instruction -> PodState -> PodState)
  -> TurnOutput
  -> PlayerState
  -> PlayerState
perPodOfPlayerI f (Vec2 i1 i2) state@PlayerState { _podStates = Vec2 s1 s2 } =
  state { _podStates = Vec2 (f i1 s1) (f i2 s2) }

-- | Apply a function on each Instruction-PodState pair of a GameState.
perPodI
  :: (Instruction -> PodState -> PodState)
  -> TurnOutput
  -> TurnOutput
  -> GameState
  -> GameState
perPodI f = perPlayerI (perPodOfPlayerI f)

-- | Check which of the two players satisfy a given predicate.
whichPlayer :: (PlayerState -> Bool) -> GameState -> Maybe PlayerIx
whichPlayer p GameState { _playerStates = Vec2 p1 p2 } | p p1 = Just Player1
                                                       | p p2 = Just Player2
                                                       | otherwise = Nothing

-- | Check if a predicate holds for any pods in a PlayerState.
anyPod :: (PodState -> Bool) -> PlayerState -> Bool
anyPod p PlayerState { _podStates = Vec2 p1 p2 } = p p1 || p p2
