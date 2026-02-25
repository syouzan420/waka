{-# LANGUAGE OverloadedStrings #-}
module WakaData where

import SDL (V2(V2))
import SDL.Video.Renderer (Renderer,Texture)
import qualified SDL.Image as I
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.Point2 (Point2(..))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Functor((<&>))
import Foreign.C.Types (CFloat,CInt)
import Control.Exception (handle,SomeException)

import Names (SpriteName(..),TileName,SenarioName,MessageName)

data WakaData = WakaData {
    wdRenderer   :: !Renderer
   ,wdGetSenario :: !(SenarioName -> Text)
   ,wdGetMessage :: !(MessageName -> Text)
   ,wdGetSprite  :: !(SpriteName -> Texture)
   ,wdGetTile    :: !(TileName -> Texture)
   ,wdGetKana    :: !(Char -> Texture)
   ,wdDouble     :: !Double
   ,wdInputMode  :: !InputMode
   ,wdFieldData  :: !FieldData
   ,wdDialog     :: ![Dialog]
   ,wdDialogBox  :: ![DialogBox]
}

data FieldData = FieldData {
    fdPlayerPos :: !(Point2 CFloat)
   ,fdPlayerImg :: !ImgType
}

data InputMode = IField | IDialog | IZyutu deriving (Show,Eq)

data DataType = DText | DPng deriving Eq

data ImgDir = ImFront | ImBack | ImLeft | ImRight deriving (Show,Eq)

data ImgLR = ImL | ImR deriving (Show,Eq) 

type ImgCount = CInt

data ImgType = ImgType !ImgDir !ImgLR !ImgCount deriving Show

data FontType = Kana | Wosite deriving Eq

data TextDir = Tate | Yoko deriving Eq

data Rect = Rect !CFloat !CFloat !CFloat !CFloat deriving Eq

type FontSize = CFloat

type Position = Point2 CFloat

data Mozi = Mozi !FontType !FontSize !Position !Char deriving Eq

data Dialog = Dialog {
  textData     :: ![Mozi]
 ,textPosition :: !CInt
 ,textCount    :: !CInt
 ,textCountMax :: !CInt
 ,isStop       :: !Bool
 ,isEnd        :: !Bool
 } deriving Eq

data DialogBox = DialogBox {
  textDir      :: !TextDir
 ,isBorder     :: !Bool
 ,dialogRect   :: !Rect
 ,fontType     :: !FontType
 ,fontSize     :: !FontSize
 ,textFeed     :: !CFloat
 ,lineFeed     :: !CFloat
} deriving Eq

defaultDialogBox :: DialogBox
defaultDialogBox = DialogBox {
  textDir = Tate
 ,isBorder = False
 ,dialogRect = Rect 0 0 100 100
 ,fontType = Kana
 ,fontSize = 20
 ,textFeed = 22
 ,lineFeed = 30
}

title :: Text
title = "わかひめ"

windowSize :: V2 CFloat
windowSize = V2 140 160 

getPaths :: (Show n,Enum n) => String -> DataType -> [(n, FilePath)]
getPaths rpath tp = [(name, "resources/" ++ toPath tp ++ "/" ++ rpath ++ "/"
                  ++ show name ++ toExt tp) | name <- [toEnum 0 ..]] 
  where toPath t = case t of DText -> "text"; DPng -> "images"
        toExt t = case t of DText -> ".txt"; DPng -> ".png"

senarioPaths :: [(SenarioName, FilePath)]
senarioPaths = getPaths "senario" DText

messagePaths :: [(MessageName, FilePath)]
messagePaths = getPaths "message" DText

defaultImagePath :: FilePath 
defaultImagePath = "resources/images/default.png"

spritePaths :: [(SpriteName, FilePath)]
spritePaths = getPaths "sprites" DPng

tilePaths :: [(TileName, FilePath)]
tilePaths = getPaths "tiles" DPng

kanaPaths :: [(Char, FilePath)]
kanaPaths = [(ch, "resources/images/fonts/font-" ++ show (fromEnum ch) ++ ".png")
             | ch <- kanaString]

kanaString :: String
kanaString = "()「」あいうえおかがきぎくぐけげこごさざしじすずせぜそぞただちぢっつづてでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもゃやゅゆょよらりるれろわゐゑをんアイウエオカガキギクグケゲコゴサザシジスズセゼソゾタダチヂッツヅテデトドナニヌネノハバパヒビピフブプヘベペホボポマミムメモャヤュユョヨラリルレロワヰヱヲン・ー"

loadWakaData :: Renderer -> IO WakaData
loadWakaData renderer = do
  let defaultText = "こんにちは"
  let textFail :: SomeException -> IO Text
      textFail e = return defaultText

  senario <- mapM (handle textFail . loadText) (M.fromList senarioPaths)

  message <- mapM (handle textFail . loadText) (M.fromList messagePaths)

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
   ,wdGetSenario = fromMaybe defaultText . (`M.lookup` senario)
   ,wdGetMessage = fromMaybe defaultText . (`M.lookup` message)
   ,wdGetSprite = fromMaybe defaultTexture . (`M.lookup` sprites)
   ,wdGetTile = fromMaybe defaultTexture . (`M.lookup` tiles)
   ,wdGetKana = fromMaybe defaultTexture . (`M.lookup` kanas)
   ,wdDouble = 0
   ,wdInputMode = IField
   ,wdFieldData = FieldData (Point2 10 10) (ImgType ImFront ImL 0)
   ,wdDialog = []
   ,wdDialogBox = []
  }

loadText :: FilePath -> IO Text
loadText fileName = B.readFile fileName <&> decodeUtf8

getSpriteName :: ImgType -> SpriteName
getSpriteName (ImgType ImFront ImL _) = PlayerFrontLeft
getSpriteName (ImgType ImFront ImR _) = PlayerFrontRight
getSpriteName (ImgType ImBack ImL _) = PlayerBackLeft
getSpriteName (ImgType ImBack ImR _) = PlayerBackRight
getSpriteName (ImgType ImLeft ImL _) = PlayerLeft
getSpriteName (ImgType ImLeft ImR _) = PlayerLeftRight
getSpriteName (ImgType ImRight ImL _) = PlayerRightLeft
getSpriteName (ImgType ImRight ImR _) = PlayerRight
