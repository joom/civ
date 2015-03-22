{-# LANGUAGE PackageImports, RecordWildCards #-}
module Main where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import Control.Monad
import qualified Data.List as L

import Board
import Drawing

data GameState =
    GameState { tileMapState :: TileMap
              , mapPosition  :: (Float, Float)
              }

initGameState :: IO GameState
initGameState = do
    randomTMap <- randomTileMap
    -- example units added
    let tMap = replaceUnit (0,0) (Just Settler)
               $ replaceUnit (0,1) (Just Worker)
               $ replaceImprovement (1,0) (Just City)
               $ randomTMap
    return $ GameState tMap (0,0)

windowWidth, windowHeight :: Int
windowWidth  = 1000
windowHeight = 800

pressedAmong :: Window -> [Key] -> IO [Key]
pressedAmong w = filterM (keyIsPressed w)

main :: IO ()
main = do
    glossState <- initState
    gameState  <- initGameState
    withWindow windowWidth windowHeight "Civ" $ \win -> do
          loop glossState gameState win
          exitSuccess
  where
    loop glossState gameState window = do
        threadDelay 20000
        pollEvents
        k <- keyIsPressed window Key'Escape
        pressedArrow <- pressedAmong window [Key'Left, Key'Right, Key'Up, Key'Down]
        let newState = moveMap pressedArrow gameState 10
        renderFrame window glossState newState
        unless k $ loop glossState newState window

-- | Moves map to the opposite direction of the key.
moveMap :: [Key] -> GameState -> Float -> GameState
moveMap keys GameState{..} i =
    GameState tileMapState (L.foldl' addOffset mapPosition keys)
  where
    addOffset (x, y) Key'Left  = (x + i, y)
    addOffset (x, y) Key'Right = (x - i, y)
    addOffset (x, y) Key'Up    = (x, y - i)
    addOffset (x, y) Key'Down  = (x, y + i)
    addOffset (x, y) _         = (x, y)

renderFrame window glossState GameState{..} = do
     let (x, y) = mapPosition
     let views = translate x y
                 $ scale 0.4 0.4
                 $ pictures (map (tilePicture tileMapState) tiles)
     displayPicture (windowWidth, windowHeight) seaColor glossState 1.0 views
     swapBuffers window
  where
    seaColor = makeColorI 10 105 148 1

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
    GLFW.setErrorCallback $ Just simpleErrorCallback
    r <- GLFW.init
    when r $ do
        m <- GLFW.createWindow width height title Nothing Nothing
        case m of
          (Just win) -> do
              GLFW.makeContextCurrent m
              f win
              GLFW.setErrorCallback $ Just simpleErrorCallback
              GLFW.destroyWindow win
          Nothing -> return ()
        GLFW.terminate
  where
    simpleErrorCallback e s =
        putStrLn $ unwords [show e, show s]

keyIsPressed :: Window -> Key -> IO Bool
keyIsPressed win key = isPress `fmap` GLFW.getKey win key

isPress :: KeyState -> Bool
isPress KeyState'Pressed   = True
isPress KeyState'Repeating = True
isPress _                  = False
