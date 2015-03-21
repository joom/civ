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
  | Tundra
    deriving (Show, Eq, Enum, Bounded)

data Resource =
  -- Strategic resources
    Horses
  | Iron
  | Coal
  | Aluminum
  | Oil
  | Uranium
  -- Luxury resources
  | Cotton
  | Spices
  | Sugar
  | Furs
  | Ivory
  | Silk
  -- Bonus resources
  | Wheat
  | Cattle
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

-- | Is supposed to output a better map in the future.
-- e.g.: less desert next to sea, less desert in poles
-- less hills/mountain next to sea, more tundra in poles
educatedTileMap :: TileMap -> IO TileMap
educatedTileMap tMap = do
    let newMap = M.mapWithKey educated tMap
    return newMap
  where
    educated key tile@(Tile t r u i) = Tile newTerrain r u i
      where
        surrounding = neighbours board key
        terrains = map (tileTerrain . (tMap !)) surrounding
        newTerrain = if length surrounding < 6
                     then Grassland else t


randomTileMap :: IO TileMap
randomTileMap = do
  r <- generate $ infiniteListOf randomTile
  educatedTileMap $ M.lazyGridMap board r
