{-# LANGUAGE RecordWildCards #-}
module Drawing where

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Math.Geometry.GridMap ((!))

import Board

-- | A basic hexagon which has a radius of 100.
hexagon :: Picture
hexagon = polygon [(0,-100),(-87,-50),(-87,50),(0,100),(87,50),(87,-50)]

-- | Location of the hexagon on coordinate system
-- if the hexagon radius is 100.
tileLocationCenter :: TileCoord -> (Float, Float)
tileLocationCenter (x, y) =
    (fromIntegral $ x * 2 * 87 + y * 87, fromIntegral $ y * 150)

-- | Renders a picture for a single tile.
tilePicture :: TileMap
            -> TileCoord -- ^ The coordinate of the tile to be rendered.
            -> Picture
tilePicture tMap t =
    translate x y
    $ pictures [ tileView tile hexagon
               , (resourceView . tileResource) tile
               , (unitView . tileUnit) tile
               , (improvementView . tileImprovement) tile
               ]
  where
    tile = tMap ! t
    (x, y) = tileLocationCenter t
    grey = makeColor (x/(x+y+0.2)) 1 (y/(x+y+0.2)) 1

-- | Adds color to a picture according to the tile terrain.
tileView :: Tile -> Picture -> Picture
tileView t = color tc
  where tc = case tileTerrain t of
               Desert    -> makeColor (206/255) (172/255) (65/255)  1  -- desert
               Grassland -> makeColor (1/255)   (166/255) (17/255)  1  -- grass
               Hill      -> makeColor (102/255) (51/255)  (0/255)   1  -- brown
               Plains    -> makeColor (215/255) (175/255) (114/255) 1  -- soil
               Tundra    -> greyN 0.8

-- | Basic drawing of a resource.
resourceView :: Maybe Resource -> Picture
resourceView Nothing = Blank
resourceView (Just r) =
    case r of
      Horses   -> color (light $ makeColor (102/255) (51/255) (0/255) 1) $ thickCircle s s
      Iron     -> color (greyN 0.5) $ thickCircle s s
      Coal     -> color black $ thickCircle s s
      Aluminum -> color white $ thickCircle s s
      Oil      -> color black $ thickCircle s s
      Uranium  -> color green $ thickCircle s s
      _        -> Blank
  where s = 12

-- | Basic drawing of a unit.
unitView :: Maybe Unit -> Picture
unitView Nothing = Blank
unitView (Just Unit{..}) =
    case unitKind of
      Settler -> color blue $ thickCircle s s
      Worker  -> color red  $ thickCircle s s
      _       -> Blank
  where s = 50

-- | Basic drawing of an improvement.
improvementView :: Maybe Improvement -> Picture
improvementView Nothing = Blank
improvementView (Just i) =
    case i of
      City    -> color black  $ thickCircle s s
      Farm    -> color yellow $ thickCircle s s
      Pasture -> color yellow $ thickCircle s s
      Mine    -> color yellow $ thickCircle s s
      Well    -> color yellow $ thickCircle s s
  where s = 20
