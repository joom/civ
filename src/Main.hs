{-# LANGUAGE PackageImports, RecordWildCards #-}
module Main where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless)

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
               $ randomTMap
    return $ GameState randomTMap (0,0)

windowWidth, windowHeight :: Int
windowWidth  = 1000
windowHeight = 800

main :: IO ()
main = do
    glossState <- initState
    gameState  <- initGameState
    withWindow windowWidth windowHeight "Civ" $ \win -> do
          loop glossState gameState win
          exitSuccess
  where loop glossState gameState window = do
            threadDelay 20000
            pollEvents
            k <- keyIsPressed window Key'Escape
            l <- keyIsPressed window Key'Left
            r <- keyIsPressed window Key'Right
            u <- keyIsPressed window Key'Up
            d <- keyIsPressed window Key'Down
            let newState = moveMap (l,r,u,d) gameState 10
            renderFrame window glossState newState
            unless k $ loop glossState newState window

-- | Moves map to the opposite direction of the key.
moveMap :: (Bool, Bool, Bool, Bool) -> GameState -> Float -> GameState
moveMap b4 GameState{..} i =
    GameState tileMapState (addOffset b4 mapPosition i)
  where
    addOffset (True, _, _, _)              (x, y) i = (x + i, y)
    addOffset (_, True, _, _)              (x, y) i = (x - i, y)
    addOffset (_, _, True, _)              (x, y) i = (x, y - i)
    addOffset (_, _, _, True)              (x, y) i = (x, y + i)
    addOffset (False, False, False, False) (x, y) _ = (x, y)

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
