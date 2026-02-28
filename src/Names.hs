module Names where

import Data.Text (Text)

type SectionName = Text 

data CharaName = NoChara 
               | Player
               | Player2
               | Person
         deriving (Enum,Eq,Ord,Read,Show)

data SpriteName = NoSprite 
                | PlayerFrontLeft
                | PlayerFrontRight
                | PlayerBackLeft
                | PlayerBackRight
                | PlayerLeftRight
                | PlayerLeft
                | PlayerRightLeft
                | PlayerRight
         deriving (Enum,Eq,Ord,Read,Show)

data TileName = NoTile
              | Brick
              | Desert
              | Forest
              | Mountain
              | Plain
              | Pond
              | Rock
              | Wood
      deriving (Enum, Eq, Ord, Read, Show)

data StageName = NoStage 
               | Title
               | Opening
               | Field
               | Battle
               | Message
               | Ending
      deriving (Eq,Show)

data SenarioName = NoSene 
                 | Start
                 | End
      deriving (Enum, Eq, Ord, Read, Show)

data MessageName = NoMessage 
                 | Yoko
                 | Wakahime
  deriving (Enum, Eq, Ord, Read, Show)
