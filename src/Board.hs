module Board where

import Control.Monad
import Control.Applicative
import Data.Maybe
import Math.Geometry.Grid.Hexagonal
import Math.Geometry.Grid.HexagonalInternal
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
    City
  | Farm
  | Pasture
  | Mine
  | Well
    deriving (Show, Eq, Enum, Bounded)

instance Arbitrary Terrain where
    arbitrary = arbitraryBoundedEnum
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

-- | Generates random tile. Should be realistic.
-- e.g.: Only a minority of tiles should have resources on them.
randomTile :: Gen Tile
randomTile = Tile <$> arbitrary
                  <*> frequency [(5, pure Nothing), (2, arbitrary)]
                  <*> pure Nothing
                  <*> pure Nothing

-- | Is supposed to output a better map in the future.
-- e.g.: less desert next to sea, less desert in poles
-- less hills/mountain next to sea, more tundra in poles
educatedTileMap :: TileMap -> M.LGridMap HexHexGrid (Gen Tile)
educatedTileMap tMap = M.mapWithKey educated tMap
  where
    educated :: TileCoord -> Tile -> Gen Tile
    educated key (Tile t r u i) = do
      t' <- if length surrounding < 6 -- next to sea
            then elements [Grassland, Plains] else return t
      r' <- if r `elem` resources -- same resource in neighbor
            then elements [r, Nothing]
            else return r
      return $ Tile t' r' u i
        where
          surrounding = neighbours board key
          terrains = map (tileTerrain . (tMap !)) surrounding
          resources = map (tileResource . (tMap !)) surrounding

sequenceMap :: Monad m => M.LGridMap HexHexGrid (m Tile) -> m TileMap
sequenceMap gMap = M.lazyGridMap board `liftM` (mapM snd . M.toList) gMap

randomTileMap :: IO TileMap
randomTileMap = do
  init <- M.lazyGridMap board `liftM` (generate . infiniteListOf) randomTile
  (generate . sequenceMap . educatedTileMap) init

-- Tile content change functions

replaceImprovement :: TileCoord -> Maybe Improvement -> TileMap -> TileMap
replaceImprovement c i tMap = M.insert c newTile tMap
  where
    newTile = (tMap ! c) { tileImprovement = i  }

replaceUnit :: TileCoord -> Maybe Unit -> TileMap -> TileMap
replaceUnit c u tMap = M.insert c newTile tMap
  where
    newTile = (tMap ! c) { tileUnit = u  }

-- | Ignores the unit in `to`, overwrites it. Use with caution.
-- Always check if `unitExists` in `to`.
moveUnit :: TileCoord -> TileCoord -> TileMap -> TileMap
moveUnit from to tMap =
    replaceUnit to unit $ replaceUnit from Nothing tMap
  where
    unit = tileUnit (tMap ! from)

moveUnitToDirection :: TileCoord -> HexDirection -> TileMap -> TileMap
moveUnitToDirection c dir tMap =
    case to of
      Just x  -> moveUnit c x tMap
      Nothing -> tMap
  where
    to :: Maybe TileCoord
    to = neighbour board c dir

newUnitPosInDirection :: TileCoord -> HexDirection -> TileCoord
newUnitPosInDirection c dir = fromMaybe c to
  where
    to = neighbour board c dir

unitExists :: TileCoord -> TileMap -> Bool
unitExists c tMap = (isJust . tileUnit) (tMap ! c)

allUnits :: TileMap -> [Unit]
allUnits = mapMaybe (tileUnit . snd) . M.toList

allCities :: TileMap -> [Improvement]
allCities = filter (== City) . mapMaybe (tileImprovement . snd) . M.toList
