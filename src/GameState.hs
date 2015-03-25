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
import Math.Geometry.GridMap ((!))

import Board
import Drawing

-- | Definition for the game state data type.
data GameState =
    GameState { tileMapState   :: TileMap
              , mapPosition    :: (Float, Float)
              , mapZoom        :: Float
              , unitPosition   :: TileCoord
              , nextUnitInLine :: Maybe TileCoord
              , blinkAnimation :: (Float, Bool)
              }
              deriving Show

-- | The main function to render the game state.
renderView :: GameState -> Picture
renderView gS@GameState{..} = views
  where
    (x, y) = mapPosition
    views  = translate x y
             $ scale mapZoom mapZoom
             $ pictures (map (tilePicture gS) tiles)

-- | Renders a picture for a single tile.
tilePicture :: GameState
            -> TileCoord -- ^ The coordinate of the tile to be rendered.
            -> Picture
tilePicture gS@GameState{..} t =
    translate x y
    $ pictures [ tileView tile hexagon
               , (resourceView . tileResource) tile
               , (unitView gS t . tileUnit) tile
               , (improvementView . tileImprovement) tile
               ]
  where
    tile = tileMapState ! t
    (x, y) = tileLocationCenter t
    grey = makeColor (x/(x+y+0.2)) 1 (y/(x+y+0.2)) 1

-- | Basic drawing of a unit.
unitView :: GameState -> TileCoord -> Maybe Unit -> Picture
unitView _ _ Nothing = Blank
unitView GameState{..} coord (Just Unit{..}) =
    case unitKind of
      Settler -> color (blinky blue) $ thickCircle s s
      Worker  -> color (blinky red)  $ thickCircle s s
      _       -> Blank
  where
    s = 50
    blinky :: Color -> Color
    blinky c =
      case nextUnitInLine of
        Just x -> if coord == x then setAlpha c (fst blinkAnimation) else c
        _      -> c

------------------------------
-- Game state change functions
------------------------------

-- | The default settings of a game state. It is semi-random.
initGameState :: IO GameState
initGameState = do
    randomTMap <- randomTileMap
    -- example units added
    let tMap = replaceUnit (0,0) (Just $ Unit Settler 1 True)
               $ replaceUnit (2,3) (Just $ Unit Worker 1 True)
               $ replaceImprovement (0,0) (Just City)
               randomTMap
    return $ GameState tMap (0,0) 0.2 (0,0) (Just (0,0)) (1.0, False)

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

-- | Takes keys about turn action and changes the game state.
turnAction :: [Key]     -- ^ Should only contain space.
           -> GameState
           -> GameState
turnAction [Key'Space] gS@GameState{..} =
    case nextUnitInLine of
        Just c -> let newTMap = deactivateNextUnitInLine c tileMapState in
                  gS { tileMapState   = newTMap
                     , nextUnitInLine = findNextUnitInLine newTMap
                     }
        _      -> gS  -- end turn or activate all units again
turnAction _ gS = gS

-- | Changes zoom scale in the game state according to the keys.
changeScale :: [Key] -- ^ Supposed to be a sub list of `-` `=`, and left and right shifts.
            -> GameState -- ^ Game state to be changed.
            -> GameState -- ^ New game state.
changeScale keys gS@GameState{..} =
    case keys of
        Key'Equal : ks -- `ks` can only contain left or right shift keys
            | (not . null) ks && mapZoom < 0.99 ->
                  gS { mapZoom = mapZoom + 0.01 }
            | otherwise -> gS
        [Key'Minus] ->
            if mapZoom > 0.02 then gS { mapZoom = mapZoom - 0.01 } else gS
        _           -> gS

-- | Set state for blink animation.
blink :: GameState -> GameState
blink gS@GameState{..} =
    gS { blinkAnimation = y  }
  where
    (x, inc) = blinkAnimation
    op = if inc then (+) else (-)
    i = 0.07 -- offset
    y = case () of
        _ | x < 0.3 && not inc -> (x + i, True)
          | x > 0.9 && inc     -> (x - i, False)
          | otherwise          -> (x `op` i, inc)
