module SpriteName where

data SpriteName = PlayerFrontLeft
                | PlayerFrontRight
                | PlayerBackLeft
                | PlayerBackRight
                | PlayerLeftRight
                | PlayerLeft
                | PlayerRightLeft
                | PlayerRight
         deriving (Enum,Eq,Ord,Read,Show)
