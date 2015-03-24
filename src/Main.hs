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
    (turnKey, turnKeySink)           <- external [] -- to move to the next unit
    (zoomKey, zoomKeySink)           <- external [] -- for zoom
    (arrowKey, arrowKeySink)         <- external [] -- for map movement
    (directionKey, directionKeySink) <- external [] -- for hex direction
    glossState <- initState
    initialGS  <- initGameState
    withWindow windowWidth windowHeight "Civ" $ \win -> do
        network <- start $ do
            gsSignal <-
                transfer4 initialGS
                          (\arrK dirK zoomK turnK gS@GameState{..} ->
                              blink
                              $ changeScale zoomK
                              $ moveMap arrK 10
                              $ turnAction turnK
                              $ moveUnitWithKey unitPosition dirK gS)
                          arrowKey directionKey zoomKey turnKey
            return $ renderFrame win glossState <$> gsSignal
        fix $ \loop -> do
            readPressedInput win turnKeys turnKeySink
            readPressedInput win zoomKeys zoomKeySink
            readPressedInput win arrowKeys arrowKeySink
            readPressedInput win directionKeys directionKeySink
            join network
            threadDelay 20000
            esc <- keyIsPressed win Key'Escape
            unless esc loop
        exitSuccess

renderFrame window glossState gameState = do
    displayPicture (windowWidth, windowHeight)
                   seaColor glossState 1.0 (renderView gameState)
    swapBuffers window
  where
    seaColor = makeColorI 10 105 148 1

-- | Checks which of the given keys are pressed.
pressedAmong :: Window -> [Key] -> IO [Key]
pressedAmong w = filterM (keyIsPressed w)

turnKeys, zoomKeys, arrowKeys, directionKeys :: [Key]
turnKeys = [Key'Space]
zoomKeys = [Key'Minus, Key'Equal, Key'LeftShift, Key'RightShift]
arrowKeys = [Key'Left, Key'Right, Key'Up, Key'Down]
directionKeys = [Key'W, Key'E, Key'D, Key'X, Key'Z, Key'A]

-- | Checks if the given keys are pressed and runs the sink.
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
