{-# LANGUAGE OverloadedStrings #-}
module WakaData where

import SDL (V2(V2))
import SDL.Video.Renderer (Renderer,Texture)
import qualified SDL.Image as I
import qualified Data.Map as M
import Data.Point2 (Point2(..))
import Foreign.C.Types (CFloat)
import Data.Maybe (fromMaybe)
import Control.Exception (handle,SomeException)

import Data.Text (Text)

import SpriteName (SpriteName(..))
import TileName (TileName)

data WakaData = WakaData {
    wdRenderer  :: !Renderer
   ,wdGetSprite :: !(SpriteName -> Texture)
   ,wdGetTile   :: !(TileName -> Texture)
   ,wdGetKana   :: !(Char -> Texture)
   ,wdDouble    :: !Double
   ,wdPlayerPos :: !(Point2 CFloat)
   ,wdPlayerImg :: !ImgType
}

data ImgLR = ImL | ImR deriving (Show,Eq) 

data ImgType = ImgType !ImgDir !ImgLR !Int deriving Show

data ImgDir = ImFront | ImBack | ImLeft | ImRight deriving (Show,Eq)

title :: Text
title = "わかひめ"

windowSize :: V2 CFloat
windowSize = V2 140 160 

defaultImagePath :: String
defaultImagePath = "resources/images/default.png"

spritePaths :: [(SpriteName, String)]
spritePaths = [(name, "resources/images/sprites/" ++ show name ++ ".png")
               | name <- [toEnum 0 ..]]

tilePaths :: [(TileName, String)]
tilePaths = [(name, "resources/images/tiles/" ++ show name ++ ".png")
             | name <- [toEnum 0 ..]]

kanaPaths :: [(Char, String)]
kanaPaths = [(ch, "resources/images/fonts/font-" ++ show (fromEnum ch) ++ ".png")
             | ch <- kanaString]

kanaString :: String
kanaString = "()「」あいうえおかがきぎくぐけげこごさざしじすずせぜそぞただちぢっつづてでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもゃやゅゆょよらりるれろわゐゑをんアイウエオカガキギクグケゲコゴサザシジスズセゼソゾタダチヂッツヅテデトドナニヌネノハバパヒビピフブプヘベペホボポマミムメモャヤュユョヨラリルレロワヰヱヲン・ー"

loadWakaData :: Renderer -> IO WakaData
loadWakaData renderer = do
  defaultTexture <- I.loadTexture renderer defaultImagePath
  
  let textureFail :: SomeException -> IO Texture
      textureFail e = return defaultTexture

  sprites <- mapM (handle textureFail . I.loadTexture renderer)
                                               (M.fromList spritePaths)
  tiles <- mapM (handle textureFail . I.loadTexture renderer)
                                               (M.fromList tilePaths)
  kanas <- mapM (handle textureFail . I.loadTexture renderer)
                                               (M.fromList kanaPaths)
  return WakaData {
    wdRenderer = renderer
   ,wdGetSprite = fromMaybe defaultTexture . (`M.lookup` sprites)
   ,wdGetTile = fromMaybe defaultTexture . (`M.lookup` tiles)
   ,wdGetKana = fromMaybe defaultTexture . (`M.lookup` kanas)
   ,wdDouble = 0
   ,wdPlayerPos = Point2 10 10
   ,wdPlayerImg = ImgType ImFront ImL 0
  }

getSpriteName :: ImgType -> SpriteName
getSpriteName (ImgType ImFront ImL _) = PlayerFrontLeft
getSpriteName (ImgType ImFront ImR _) = PlayerFrontRight
getSpriteName (ImgType ImBack ImL _) = PlayerBackLeft
getSpriteName (ImgType ImBack ImR _) = PlayerBackRight
getSpriteName (ImgType ImLeft ImL _) = PlayerLeft
getSpriteName (ImgType ImLeft ImR _) = PlayerLeftRight
getSpriteName (ImgType ImRight ImL _) = PlayerRightLeft
getSpriteName (ImgType ImRight ImR _) = PlayerRight
