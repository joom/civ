{-# LANGUAGE ScopedTypeVariables #-}
module Board where

import Control.Monad
import Control.Applicative
import Math.Geometry.Grid.Hexagonal
import Math.Geometry.GridInternal
import Math.Geometry.GridMap ((!))
import qualified Math.Geometry.GridMap as M
import qualified Math.Geometry.GridMap.Lazy as M
import System.Random

type Tile    = (Int, Int)
type TileMap = M.LGridMap HexHexGrid Terrain

data Terrain =
    Desert
  | Grassland
  | Hill
  | Plains
    deriving (Show, Eq, Enum, Bounded)

board :: HexHexGrid
board = hexHexGrid 10

tiles :: [Tile]
tiles = indices board

tileMap :: TileMap
tileMap = M.lazyGridMap board $ cycle [Desert, Grassland, Hill, Plains]

randomTerrain :: IO Terrain
randomTerrain = toEnum <$> randomRIO (fromEnum min, fromEnum max)
  where min = minBound :: Terrain
        max = maxBound :: Terrain

randomTileMap :: IO TileMap
randomTileMap = do
  r <- replicateM (tileCount board) randomTerrain
  return $ M.lazyGridMap board r
