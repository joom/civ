{-# LANGUAGE PackageImports, RecordWildCards #-}
module GameState where

import "GLFW-b" Graphics.UI.GLFW as GLFW
import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture

import Control.Applicative
import qualified Data.List as L
import Math.Geometry.Grid.Hexagonal
import Math.Geometry.Grid.HexagonalInternal

import Board
import Drawing

data GameState =
    GameState { tileMapState :: TileMap
              , mapPosition  :: (Float, Float)
              , unitPosition :: TileCoord
              }

initGameState :: IO GameState
initGameState = do
    randomTMap <- randomTileMap
    -- example units added
    let tMap = replaceUnit (0,0) (Just Settler)
               $ replaceImprovement (0,0) (Just City)
               randomTMap
    return $ GameState tMap (0,0) (0,0)

-- | Moves map to the opposite direction of the key, by the float number given.
moveMap :: [Key] -> Float -> GameState -> GameState
moveMap keys i gs@GameState{..} =
    gs { mapPosition = L.foldl' addOffset mapPosition keys }
  where
    addOffset (x, y) Key'Left  = (x + i, y)
    addOffset (x, y) Key'Right = (x - i, y)
    addOffset (x, y) Key'Up    = (x, y - i)
    addOffset (x, y) Key'Down  = (x, y + i)
    addOffset (x, y) _         = (x, y)

moveUnitWithKey :: TileCoord -> [Key] -> GameState -> GameState
moveUnitWithKey c [k] gS@GameState{..} =
    gS { tileMapState = moveUnitToDirection c dir tileMapState
       , unitPosition = newUnitPosInDirection c dir
       }
  where dir = keyToDirection k
moveUnitWithKey _ _ gS = gS

-- | Assigns a hexagonal direction to the keys W,E,D,X,Z,A.
keyToDirection :: Key -> HexDirection
keyToDirection k =
    case k of
      Key'W -> Northwest
      Key'E -> Northeast
      Key'D -> East
      Key'X -> Southeast
      Key'Z -> Southwest
      Key'A -> West
      _     -> error "No hexagonal direction assigned for this key."
