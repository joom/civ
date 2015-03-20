module Drawing where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Math.Geometry.GridMap ((!))

import Board

hexagon :: Picture
hexagon = polygon [(0,-100),(-87,-50),(-87,50),(0,100),(87,50),(87,-50)]

-- | Location of the hexagon on coordinate system
-- if the hexagon radius is 100.
tileLocationCenter :: Tile -> (Float, Float)
tileLocationCenter (x, y) =
    (fromIntegral $ x * 2 * 87 + y * 87, fromIntegral $ y * 150)

tilePicture :: TileMap -> Tile -> Picture
tilePicture tMap t = terrainView (tMap ! t) $ translate x y hexagon
  where
    (x, y) = tileLocationCenter t
    grey = makeColor (x/(x+y+0.2)) 1 (y/(x+y+0.2)) 1

terrainView :: Terrain -> Picture -> Picture
terrainView t = color tc
  where tc = case t of
               Desert    -> makeColor (206/255) (172/255) (65/255)  1  -- desert
               Grassland -> makeColor (1/255)   (166/255) (17/255)  1  -- grass
               Hill      -> makeColor (102/255) (51/255)  (0/255)   1  -- brown
               Plains    -> makeColor (215/255) (175/255) (114/255) 1  -- soil

