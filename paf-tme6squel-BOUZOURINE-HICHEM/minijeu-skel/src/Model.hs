
module Model where

import SDL

import Keyboard (Keyboard)
import qualified Keyboard as K

data GameState = GameState { persoX :: Int
                           , persoY :: Int
                           , speed :: Int }
  deriving (Show)


initGameState :: GameState
initGameState = GameState 200 300 4

moveLeft :: GameState -> GameState
moveLeft gs@(GameState x _ s) 
  | x > 0 = gs{persoX = x - s}
  | otherwise = gs

moveRight :: GameState -> GameState
moveRight gs@(GameState x _ s) 
  | x < 540 = gs{persoX = x + s}
  | otherwise = gs
                              
moveUp :: GameState -> GameState
moveUp gs@(GameState _ y s) 
  | y > 0 = gs{persoY = y - s}
  | otherwise = gs

moveDown :: GameState -> GameState
moveDown gs@(GameState _ y s) 
  | y < 380 = gs{persoY = y + s}
  | otherwise = gs

-- source d'inspiration : github
insideGameState :: Maybe (V2 Int) -> GameState -> Bool 
insideGameState coords gs@(GameState px py _) = 
  case coords of
    Just (V2 x y) -> px <= x && x <= (px + 100) && py <= y && y <= (py + 100)
    Nothing -> False

gameStep :: RealFrac a => GameState -> Keyboard -> a -> GameState
gameStep gstate kbd deltaTime =
  let modif = (if K.keypressed KeycodeLeft kbd
               then moveLeft else id)
              .
              (if K.keypressed KeycodeRight kbd
               then moveRight else id)
              .
              (if K.keypressed KeycodeUp kbd
               then moveUp else id)
              .
              (if K.keypressed KeycodeDown kbd
               then moveDown else id)

  in modif gstate
