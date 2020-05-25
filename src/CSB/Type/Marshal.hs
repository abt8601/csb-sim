module CSB.Type.Marshal where

import           Foreign.Ptr
import           Foreign.Storable

import           CSB.Type
import           Data.Vec2

instance Storable GameState where
  sizeOf _ = 296
  alignment _ = 8
  peek ptr =
    GameState <$> peekByteOff ptr 0 <*> (toEnum <$> peekByteOff ptr 288)
  poke ptr (GameState s st) =
    pokeByteOff ptr 0 s >> pokeByteOff ptr 288 (fromEnum st)

instance Storable PlayerState where
  sizeOf _ = 144
  alignment _ = 8
  peek ptr =
    PlayerState
      <$> peekByteOff ptr 0
      <*> (toEnum <$> peekByteOff ptr 128)
      <*> peekByteOff ptr 136
  poke ptr (PlayerState p b t) =
    pokeByteOff ptr 0 p
      >> pokeByteOff ptr 128 (fromEnum b)
      >> pokeByteOff ptr 136 t

instance Storable PodState where
  sizeOf _ = 64
  alignment _ = 8
  peek ptr =
    PodState
      <$> peekByteOff ptr 0
      <*> peekByteOff ptr 16
      <*> peekByteOff ptr 32
      <*> peekByteOff ptr 40
      <*> peekByteOff ptr 48
      <*> peekByteOff ptr 56
  poke ptr (PodState r v theta i l s) =
    pokeByteOff ptr 0 r
      >> pokeByteOff ptr 16 v
      >> pokeByteOff ptr 32 theta
      >> pokeByteOff ptr 40 i
      >> pokeByteOff ptr 48 l
      >> pokeByteOff ptr 56 s

instance (Storable a) => Storable (Vec2 a) where
  sizeOf v = 2 * sizeOf (elemV v)
  alignment v = alignment (elemV v)
  peek ptr = Vec2 <$> peekByteOff ptr 0 <*> peekByteOff ptr off
    where off = sizeOf (derefV ptr)
  poke ptr (Vec2 x y) = pokeByteOff ptr 0 x >> pokeByteOff ptr off y
    where off = sizeOf x

-- | Utility function that makes instance Storable (Vec2 a) work.
elemV :: Vec2 a -> a
elemV = undefined

-- | Utility function that makes instance Storable (Vec2 a) work.
derefV :: Ptr (Vec2 a) -> a
derefV = undefined
