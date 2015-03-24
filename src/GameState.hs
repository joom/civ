{-# LANGUAGE PackageImports, RecordWildCards, PatternGuards #-}
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

-- | Definition for the game state data type.
data GameState =
    GameState { tileMapState   :: TileMap
              , mapPosition    :: (Float, Float)
              , mapZoom        :: Float
              , unitPosition   :: TileCoord
              , nextUnitInLine :: Maybe TileCoord
              }
              deriving Show

-- | The main function to render the game state.
renderView :: GameState -> Picture
renderView = undefined

-- | The default settings of a game state. It is semi-random.
initGameState :: IO GameState
initGameState = do
    randomTMap <- randomTileMap
    -- example units added
    let tMap = replaceUnit (0,0) (Just $ Unit Settler 1 True)
               $ replaceImprovement (0,0) (Just City)
               randomTMap
    return $ GameState tMap (0,0) 0.2 (0,0) Nothing

-- | Moves map to the opposite direction of the key, by the float number given.
moveMap :: [Key]     -- ^ Arrow keys. Other keys are ignored.
        -> Float     -- ^ Translation offset.
        -> GameState -- ^ Game state to be changed.
        -> GameState -- ^ New game state.
moveMap keys i gs@GameState{..} =
    gs { mapPosition = L.foldl' addOffset mapPosition keys }
  where
    addOffset (x, y) Key'Left  = (x + i, y)
    addOffset (x, y) Key'Right = (x - i, y)
    addOffset (x, y) Key'Up    = (x, y - i)
    addOffset (x, y) Key'Down  = (x, y + i)
    addOffset (x, y) _         = (x, y)

-- | Moves the unit in the given coordinate according to the key.
moveUnitWithKey :: TileCoord -- ^ The coordinate of the unit to be moved.
                -> [Key]     -- ^ The direction keys to determine the direction.
                -> GameState -- ^ Game state to be changed.
                -> GameState -- ^ New game state.
moveUnitWithKey c [k] gS@GameState{..} =
    gS { tileMapState = moveUnitToDirection c dir tileMapState
       , unitPosition = newUnitPosInDirection c dir
       }
  where dir = keyToDirection k
moveUnitWithKey _ _ gS = gS

-- | Assigns a hexagonal direction to the keys W,E,D,X,Z,A.
-- Note that this is a partial function and it fails on other keys.
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

-- | Changes zoom scale in the game state according to the keys.
changeScale :: [Key] -- ^ Supposed to be a sub list of `-` `=`, and left and right shifts.
            -> GameState -- ^ Game state to be changed.
            -> GameState -- ^ New game state.
changeScale keys gS@GameState{..} =
    case keys of
        Key'Equal : ks -- `ks` can only contain left or right shift keys
            | (not . null) ks ->
                if mapZoom < 0.99 then gS { mapZoom = mapZoom + 0.01 } else gS
            | otherwise       -> gS
        [Key'Minus] ->
            if mapZoom > 0.02 then gS { mapZoom = mapZoom - 0.01 } else gS
        _           -> gS
