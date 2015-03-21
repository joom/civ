module Board where

import Control.Monad
import Control.Applicative
import Math.Geometry.Grid.Hexagonal
import Math.Geometry.GridInternal
import Math.Geometry.GridMap ((!))
import qualified Math.Geometry.GridMap as M
import qualified Math.Geometry.GridMap.Lazy as M
import Test.QuickCheck.Arbitrary -- to randomly generate values
import Test.QuickCheck.Gen

type TileCoord = (Int, Int)
type TileMap   = M.LGridMap HexHexGrid Tile

data Tile =
    Tile { tileTerrain     :: Terrain
         , tileResource    :: Maybe Resource
         , tileUnit        :: Maybe Unit
         , tileImprovement :: Maybe Improvement
         }
    deriving (Show, Eq)

data Terrain =
    Desert
  | Grassland
  | Hill
  | Plains
    deriving (Show, Eq, Enum, Bounded)

data Resource =
    Horses
  | Iron
  | Coal
  | Aluminum
  | Oil
  | Uranium
    deriving (Show, Eq, Enum, Bounded)

data Unit =
    Settler
  | Worker
  | Warrior
  | Archer
    deriving (Show, Eq, Enum, Bounded)

data Improvement =
    Farm
  | Pasture
  | Mine
  | Well
    deriving (Show, Eq, Enum, Bounded)

instance Arbitrary Resource where
    arbitrary = arbitraryBoundedEnum
instance Arbitrary Unit where
    arbitrary = arbitraryBoundedEnum
instance Arbitrary Improvement where
    arbitrary = arbitraryBoundedEnum

board :: HexHexGrid
board = hexHexGrid 10

tiles :: [TileCoord]
tiles = indices board

numTiles :: Int
numTiles = tileCount board

randomTile :: Gen Tile
randomTile = Tile <$> arbitraryBoundedEnum
                  <*> arbitrary
                  <*> pure Nothing
                  <*> pure Nothing

randomTileMap :: IO TileMap
randomTileMap = do
  r <- generate $ infiniteListOf randomTile
  return $ M.lazyGridMap board r
