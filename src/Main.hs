{-# LANGUAGE PackageImports #-}
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
              }

initGameState :: IO GameState
initGameState = do
    randomTMap <- randomTileMap
    return $ GameState randomTMap

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
            renderFrame window glossState gameState
            k <- keyIsPressed window Key'Escape
            unless k $ loop glossState gameState window

renderFrame window glossState gameState = do
     let tMap = tileMapState gameState
     displayPicture (windowWidth, windowHeight) white glossState 1.0
      $ scale 0.4 0.4
      $ pictures (map (tilePicture tMap) tiles)
     swapBuffers window

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
