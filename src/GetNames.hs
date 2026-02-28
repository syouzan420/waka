module GetNames where

import WakaData (ImgType(..))
import Names (SpriteName)

getSpriteName :: ImgType -> SpriteName
getSpriteName (ImgType chara dir lr) = 
  toEnum (1+(fromEnum chara-1)*8+(fromEnum dir*2)+fromEnum lr) :: SpriteName
