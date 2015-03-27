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
import Textures

-- | Definition for the game state data type.
data GameState =
    GameState { tileMapState   :: TileMap
              , mapPosition    :: (Float, Float)
              , mapZoom        :: Float
              , civColor       :: Color
              , nextUnitInLine :: Maybe TileCoord
              , blinkAnimation :: (Float, Bool)
              }
              deriving Show

-- | The main function to render the game state.
renderView :: TextureMap -> GameState -> Picture
renderView txMap gS@GameState{..} = views
  where
    (x, y) = mapPosition
    views  = translate x y
             $ scale mapZoom mapZoom
             $ pictures (map (tilePicture txMap gS) tiles)

-- | Renders a picture for a single tile.
tilePicture :: TextureMap
            -> GameState
            -> TileCoord -- ^ The coordinate of the tile to be rendered.
            -> Picture
tilePicture txMap gS@GameState{..} t =
    translate x y
    $ pictures [ tileView txMap tile
               , (improvementView txMap . tileImprovement) tile
               , (resourceView txMap . tileResource) tile
               , (unitView txMap gS t . tileUnit) tile
               ]
  where
    tile = tileMapState ! t
    (x, y) = tileLocationCenter t
    grey = makeColor (x/(x+y+0.2)) 1 (y/(x+y+0.2)) 1

-- | Basic drawing of a unit.
unitView :: TextureMap -> GameState -> TileCoord -> Maybe Unit -> Picture
unitView _ _ _ Nothing = Blank
unitView txMap GameState{..} coord (Just Unit{..}) =
  pictures [
    case unitKind of
      Settler -> "settler" `from` txMap
      Worker  -> "worker" `from` txMap
      -- Worker  -> color (blinky red)  $ "worker" `from` txMap
      _       -> Blank
    , color (blinky civColor) $ thickCircle 80 10 ]
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
    return $ GameState tMap (0,0) 0.6 blue (Just (0,0)) (1.0, False)

-- | Moves map to the opposite direction of the key, by the float number given.
moveMap :: (Float, Float) -- ^ Indicates directions coming from getCursorKeyDirections.
        -> Float     -- ^ Translation offset.
        -> GameState -- ^ Game state to be changed.
        -> GameState -- ^ New game state.
moveMap (x,y) i gs@GameState{..} =
    gs { mapPosition = (x' + x * i, y' + y * i) }
  where (x', y') = mapPosition

-- | Moves the unit in the given coordinate according to the key.
moveUnitWithKey :: Maybe TileCoord -- ^ The coordinate of the unit to be moved.
                -> [Key]           -- ^ The direction keys to determine the direction.
                -> GameState       -- ^ Game state to be changed.
                -> GameState       -- ^ New game state.
moveUnitWithKey mCoord [k] gS@GameState{..} =
    case mCoord of
      Nothing -> gS
      Just c ->
          gS { tileMapState = moveUnitToDirection c dir tileMapState
             , nextUnitInLine = findNextUnitInLine
                                $ deactivateNextUnitInLine c tileMapState
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

-- | Changes zoom scale in the game state with respect to a coefficient.
changeScale :: Float     -- ^ The coefficient to respond to.
            -> GameState -- ^ Game state to be changed.
            -> GameState -- ^ New game state.
changeScale coeff gS@GameState{..} =
    gS { mapZoom = limited }
  where
    newZoom = mapZoom * ((coeff + 100.0) / 100)
    limited | newZoom < 0.2 = 0.2
            | newZoom > 0.9 = 0.9
            | otherwise     = newZoom

-- | Set state for blink animation.
blink :: GameState -> GameState
blink gS@GameState{..} =
    gS { blinkAnimation = y  }
  where
    (x, inc) = blinkAnimation
    op = if inc then (+) else (-)
    i = 0.05 -- offset
    y = case () of
        _ | x < 0.3 && not inc -> (x + i, True)
          | x > 0.9 && inc     -> (x - i, False)
          | otherwise          -> (x `op` i, inc)
