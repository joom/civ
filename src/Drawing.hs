module Drawing where

import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Math.Geometry.GridMap ((!))

import Board
import Textures

-- | A basic hexagon which has a radius of 100.
hexagon :: Picture
hexagon = polygon [(0,-100),(-87,-50),(-87,50),(0,100),(87,50),(87,-50)]

-- | Location of the hexagon on coordinate system
-- if the hexagon radius is 100.
tileLocationCenter :: TileCoord -> (Float, Float)
tileLocationCenter (x, y) =
    (fromIntegral $ x * 2 * 87 + y * 87, fromIntegral $ y * 150)

-- | Adds color to a picture according to the tile terrain.
tileView :: TextureMap -> Tile -> Picture
tileView txMap t = tc
  where tc = case tileTerrain t of
               Desert    -> "desert" `from` txMap
               Grassland -> "grass" `from` txMap
               Hill      -> "hill" `from` txMap
               Plains    -> "plains" `from` txMap
               Tundra    -> "tundra" `from` txMap

-- | Basic drawing of a resource.
resourceView :: TextureMap -> Maybe Resource -> Picture
resourceView _ Nothing = Blank
resourceView txMap (Just r) =
    case r of
      Horses   -> color (light $ makeColor (102/255) (51/255) (0/255) 1) $ thickCircle s s
      Iron     -> color (greyN 0.5) $ thickCircle s s
      Coal     -> color black $ thickCircle s s
      Aluminum -> color white $ thickCircle s s
      Oil      -> color black $ thickCircle s s
      Uranium  -> color green $ thickCircle s s
      _        -> Blank
  where s = 12

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

-- | Changes the alpha value of a color to get a new color.
setAlpha :: Color -> Float -> Color
setAlpha color = makeColor r g b
  where (r, g, b, _) = rgbaOfColor color
