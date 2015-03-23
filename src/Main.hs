{-# LANGUAGE PackageImports, RecordWildCards #-}
module Main where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import FRP.Elerea.Simple

import System.Exit (exitSuccess)
import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Fix (fix)
import Control.Applicative

import Board
import Drawing
import GameState

windowWidth, windowHeight :: Int
windowWidth  = 1000
windowHeight = 800

main :: IO ()
main = do
    (arrowKey, arrowKeySink)         <- external [] -- for map movement
    (directionKey, directionKeySink) <- external [] -- for hex direction
    glossState <- initState
    initialGS  <- initGameState
    withWindow windowWidth windowHeight "Civ" $ \win -> do
        network <- start $ do
            gsSignal <-
                transfer2 initialGS
                          (\arrK dirK gS@GameState{..} ->
                              moveMap arrK 10
                              $ moveUnitWithKey unitPosition dirK gS)
                          arrowKey directionKey
            return $ renderFrame win glossState <$> gsSignal
        fix $ \loop -> do
            readPressedInput win arrowKeys arrowKeySink
            readPressedInput win directionKeys directionKeySink
            join network
            threadDelay 20000
            esc <- keyIsPressed win Key'Escape
            unless esc loop
        exitSuccess

renderFrame window glossState GameState{..} = do
    let (x, y) = mapPosition
    let views = translate x y
                $ scale 0.4 0.4
                $ pictures (map (tilePicture tileMapState) tiles)
    displayPicture (windowWidth, windowHeight) seaColor glossState 1.0 views
    swapBuffers window
  where
    seaColor = makeColorI 10 105 148 1

pressedAmong :: Window -> [Key] -> IO [Key]
pressedAmong w = filterM (keyIsPressed w)

arrowKeys, directionKeys :: [Key]
arrowKeys = [Key'Left, Key'Right, Key'Up, Key'Down]
directionKeys = [Key'W, Key'E, Key'D, Key'X, Key'Z, Key'A]

readPressedInput :: Window -> [Key] -> ([Key] -> IO ()) -> IO ()
readPressedInput window keys sink = do
    pollEvents
    pressedAmong window keys >>= sink

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
