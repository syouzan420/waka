module Names where

data SpriteName = PlayerFrontLeft
                | PlayerFrontRight
                | PlayerBackLeft
                | PlayerBackRight
                | PlayerLeftRight
                | PlayerLeft
                | PlayerRightLeft
                | PlayerRight
         deriving (Enum,Eq,Ord,Read,Show)

data TileName = Brick
              | Desert
              | Forest
              | Mountain
              | Plain
              | Pond
              | Rock
              | Wood
      deriving (Enum, Eq, Ord, Read, Show)

data SenarioName = Start
                 | End
      deriving (Enum, Eq, Ord, Read, Show)

data MessageName = Yoko
                 | Wakahime
  deriving (Enum, Eq, Ord, Read, Show)
