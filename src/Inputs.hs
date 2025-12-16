module Inputs(initInput,readInputs,Inputs(..)) where

import SDL.Input.Keyboard.Codes

data Inputs = Inputs {
  inpA :: !Bool
 ,inpB :: !Bool
 ,inpUp :: !Bool
 ,inpDown :: !Bool
 ,inpLeft :: !Bool
 ,inpRight :: !Bool
 ,inpQ :: !Bool
}

initInput :: Inputs
initInput = Inputs {
  inpA = False
 ,inpB = False
 ,inpUp = False
 ,inpDown = False
 ,inpLeft = False
 ,inpRight = False
 ,inpQ = False
}
  
readInputs :: (Scancode -> Bool) -> Inputs
readInputs keyDown = Inputs {
  inpA = a
 ,inpB = b
 ,inpUp = up
 ,inpDown = down
 ,inpLeft = left
 ,inpRight = right
 ,inpQ = q
}
  where
    a = keyDown ScancodeSpace
    b = keyDown ScancodeReturn
    up = keyDown ScancodeK || keyDown ScancodeUp
    down = keyDown ScancodeJ || keyDown ScancodeDown
    left = keyDown ScancodeH || keyDown ScancodeLeft
    right = keyDown ScancodeL || keyDown ScancodeRight
    q = keyDown ScancodeQ || keyDown ScancodeEscape
