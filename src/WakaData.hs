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

import Names (CharaName(..),SpriteName(..),TileName,StageName(..),SenarioName(..)
             ,MessageName,SectionName)

data WakaData = WakaData {
    wdRenderer   :: !Renderer
   ,wdGetSenario :: !(SenarioName -> Text)
   ,wdGetMessage :: !(MessageName -> Text)
   ,wdGetSprite  :: !(SpriteName -> Texture)
   ,wdGetTile    :: !(TileName -> Texture)
   ,wdGetKana    :: !(Char -> Texture)
   ,wdProgress   :: !Progress
   ,wdDouble     :: !Double
   ,wdInputMode  :: !InputMode
   ,wdFieldData  :: !FieldData
   ,wdSeneData   :: !SeneData
   ,wdDialog     :: ![Dialog]
   ,wdDialogBox  :: ![DialogBox]
}

type Fire = Bool

type Senario = Text

type Sene = (SenarioName,SectionName)

data Progress = Progress !Fire !StageName ![Sene] deriving (Show,Eq)

data SeneData = SeneData {
    sene        :: !Sene
   ,senario     :: !Senario
} deriving (Show,Eq)

data FieldData = FieldData {
    fdPlayerPos :: !(Point2 CFloat)
   ,fdPlayerImg :: !(ImgType,ImgCount)
} deriving (Show,Eq)

data InputMode = IField | IDialog | IZyutu deriving (Show,Eq)

data DataType = DText | DPng deriving Eq

data ImgDir = ImFront | ImBack | ImLeft | ImRight deriving (Show,Eq,Enum,Ord)

data ImgLR = ImL | ImR deriving (Show,Eq,Enum,Ord) 

type ImgCount = CInt

data ImgType = ImgType !CharaName !ImgDir !ImgLR deriving (Show,Eq)

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

defaultDialog :: Dialog
defaultDialog = Dialog {
  textData     = []
 ,textPosition = 0
 ,textCount    = 0
 ,textCountMax = 5
 ,isStop       = False
 ,isEnd        = False
}

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
                  ++ show name ++ toExt tp) | name <- [toEnum 1 ..]] 
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
      textFail _ = return defaultText

  senarios <- mapM (handle textFail . loadText) (M.fromList senarioPaths)

  messages <- mapM (handle textFail . loadText) (M.fromList messagePaths)

  defaultTexture <- I.loadTexture renderer defaultImagePath
  
  let textureFail :: SomeException -> IO Texture
      textureFail _ = return defaultTexture

  sprites <- mapM (handle textureFail . I.loadTexture renderer)
                                               (M.fromList spritePaths)
  tiles <- mapM (handle textureFail . I.loadTexture renderer)
                                               (M.fromList tilePaths)
  kanas <- mapM (handle textureFail . I.loadTexture renderer)
                                               (M.fromList kanaPaths)
  return WakaData {
    wdRenderer = renderer
   ,wdGetSenario = fromMaybe defaultText . (`M.lookup` senarios)
   ,wdGetMessage = fromMaybe defaultText . (`M.lookup` messages)
   ,wdGetSprite = fromMaybe defaultTexture . (`M.lookup` sprites)
   ,wdGetTile = fromMaybe defaultTexture . (`M.lookup` tiles)
   ,wdGetKana = fromMaybe defaultTexture . (`M.lookup` kanas)
   ,wdProgress = Progress True Opening [(Start,"start")]
   ,wdDouble = 0
   ,wdInputMode = IField
   ,wdFieldData = FieldData {fdPlayerPos=Point2 10 10
                            ,fdPlayerImg= (ImgType Player ImFront ImL,0)}
   ,wdSeneData = SeneData {sene =(NoSene,"") ,senario=""}
   ,wdDialog = []
   ,wdDialogBox = []
  }

loadText :: FilePath -> IO Text
loadText fileName = B.readFile fileName <&> decodeUtf8

