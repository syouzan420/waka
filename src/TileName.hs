module TileName where

data TileName = Brick
              | Desert
              | Forest
              | Mountain
              | Plain
              | Pond
              | Rock
              | Wood
      deriving (Enum, Eq, Ord, Read, Show)
