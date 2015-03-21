module Drawing where

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Math.Geometry.GridMap ((!))

import Board

hexagon :: Picture
hexagon = polygon [(0,-100),(-87,-50),(-87,50),(0,100),(87,50),(87,-50)]

-- | Location of the hexagon on coordinate system
-- if the hexagon radius is 100.
tileLocationCenter :: TileCoord -> (Float, Float)
tileLocationCenter (x, y) =
    (fromIntegral $ x * 2 * 87 + y * 87, fromIntegral $ y * 150)

tilePicture :: TileMap -> TileCoord -> Picture
tilePicture tMap t =
    translate x y
    $ pictures [ tileView tile hexagon
               , scale 0.2 0.2 $ (resourceView . tileResource) tile
               ]
  where
    tile = tMap ! t
    (x, y) = tileLocationCenter t
    grey = makeColor (x/(x+y+0.2)) 1 (y/(x+y+0.2)) 1

tileView :: Tile -> Picture -> Picture
tileView t = color tc
  where tc = case tileTerrain t of
               Desert    -> makeColor (206/255) (172/255) (65/255)  1  -- desert
               Grassland -> makeColor (1/255)   (166/255) (17/255)  1  -- grass
               Hill      -> makeColor (102/255) (51/255)  (0/255)   1  -- brown
               Plains    -> makeColor (215/255) (175/255) (114/255) 1  -- soil
               Tundra    -> greyN 0.8

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
  where s = 60

bitmaps :: IO [Picture]
bitmaps = sequence [
      loadBMP "assets/iron.bmp"
    , loadBMP "assets/coal.bmp"
    , loadBMP "assets/uranium.bmp"
    ]
