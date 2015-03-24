module Board where

import Control.Monad
import Control.Applicative
import Control.Arrow
import Data.Maybe
import Math.Geometry.Grid.Hexagonal
import Math.Geometry.Grid.HexagonalInternal
import Math.Geometry.GridInternal
import Math.Geometry.GridMap ((!))
import qualified Math.Geometry.GridMap as M
import qualified Math.Geometry.GridMap.Lazy as M
import Test.QuickCheck.Arbitrary -- to randomly generate values
import Test.QuickCheck.Gen

-- | Alias to be able to change the type of map quickly.
type MapGrid   = HexHexGrid
-- | Tile coordinate alias.
type TileCoord = (Int, Int)
-- | Alias for a mapping between tile coordinates and tiles.
type TileMap   = M.LGridMap MapGrid Tile

-- | Basic map tiles. Just contains tile coordinates.
-- This is referred by many functions, it's a constant.
board :: MapGrid
board = hexHexGrid 10

-- | Convenience name for all tile coordinates.
tiles :: [TileCoord]
tiles = indices board

-- | Convenience name for the number of tile coordinates.
numTiles :: Int
numTiles = tileCount board

-- | Definition for the type that contains the information
-- needed for a tile. Note that the list of units and cities of a player
-- are also generated from this. (from TileMap to be clearer)
data Tile =
    Tile { tileTerrain     :: Terrain
         , tileResource    :: Maybe Resource
           -- | Structurally implies that there cannot be stacked units.
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

-- | Definition for the type that contains the information
-- needed for a unit and for turns.
data Unit =
    Unit { -- | Kind of unit. (e.g.: Settler)
           unitKind         :: UnitKind
           -- | # of movements left. If it becomes 0, then the changing
           -- function should set `unitInLine` to False.
         , unitMovementLeft :: Int
           -- | The way to determine if the unit is to be played
           -- in this turn yet. If there is no movement left, this must be
           -- set to False. If the user decides to skip the unit, it can be
           -- set to False even if there is movement left.
         , unitInLine       :: Bool
         }
         deriving (Show, Eq)

data UnitKind =
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
    arbitrary = Unit <$> arbitraryBoundedEnum
                     <*> pure 1
                     <*> pure True
instance Arbitrary Improvement where
    arbitrary = arbitraryBoundedEnum


-- | Generates random tile. Should be realistic.
-- e.g.: Only a minority of tiles should have resources on them.
randomTile :: Gen Tile
randomTile = Tile <$> arbitrary
                  <*> frequency [(5, pure Nothing), (2, arbitrary)]
                  <*> pure Nothing
                  <*> pure Nothing

-- | Makes a TileMap more realistic.
-- Is supposed to output a better map in the future.
-- e.g.: less desert next to sea, less desert in poles
-- less hills/mountain next to sea, more tundra in poles
educatedTileMap :: TileMap -> M.LGridMap MapGrid (Gen Tile)
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

-- | Takes an output of `educatedTileMap`, and puts the entire
-- TileMap into the monadic context. Similar to `Control.Monad.sequence`.
sequenceMap :: Monad m => M.LGridMap MapGrid (m Tile) -> m TileMap
sequenceMap gMap = M.lazyGridMap board `liftM` (mapM snd . M.toList) gMap

-- | Generates completely random TileMap. Probably unrealistically.
randomTileMap :: IO TileMap
randomTileMap = do
  init <- M.lazyGridMap board `liftM` (generate . infiniteListOf) randomTile
  (generate . sequenceMap . educatedTileMap) init

--------------------------------
-- Tile content change functions
--------------------------------

-- | Changes the improvement on a tile coordinate.
replaceImprovement :: TileCoord         -- ^ Tile coordinate to be changed.
                   -> Maybe Improvement -- ^ The new value.
                   -> TileMap           -- ^ The tile map to be changed.
                   -> TileMap           -- ^ Resulting tile map with the new improvement.
replaceImprovement c i tMap = M.insert c newTile tMap
  where
    newTile = (tMap ! c) { tileImprovement = i  }

replaceUnit :: TileCoord  -- ^ Tile coordinate to be changed.
            -> Maybe Unit -- ^ The new value.
            -> TileMap    -- ^ The tile map to be changed.
            -> TileMap    -- ^ Resulting tile map with the new unit.
replaceUnit c u tMap = M.insert c newTile tMap
  where
    newTile = (tMap ! c) { tileUnit = u  }

-- | Moves the unit in a given coordinate to another coordinate.
-- Ignores the unit in `to`, overwrites it. Use with caution.
-- Always check if `unitExists` in `to`.
moveUnit :: TileCoord -- ^ The coordinate the unit is being moved from.
         -> TileCoord -- ^ The coordinate the unit is being moved to.
         -> TileMap   -- ^ The tile map to be changed.
         -> TileMap   -- ^ Resulting map with the moved unit.
moveUnit from to tMap =
    replaceUnit to unit $ replaceUnit from Nothing tMap
  where
    unit = tileUnit (tMap ! from)

-- | Moves the unit in a given coordinate to a given direction.
-- Ignores the unit in the direction.
moveUnitToDirection :: TileCoord    -- ^ The coordinate the unit is being moved from.
                    -> HexDirection -- ^ The direction the unit is being moved to.
                    -> TileMap      -- ^ The tile map to be changed.
                    -> TileMap      -- ^ Resulting map with the moved unit.
moveUnitToDirection c dir tMap =
    case to of
      Just x  -> moveUnit c x tMap
      Nothing -> tMap
  where
    to :: Maybe TileCoord
    to = neighbour board c dir

-- | Gives the coordinate of a neighbor in the given direction.
newUnitPosInDirection :: TileCoord    -- ^ The coordinate of the main tile.
                      -> HexDirection -- ^ The direction to check the neighbor.
                      -> TileCoord    -- ^ The coordinate of the neighbor.
newUnitPosInDirection c dir = fromMaybe c to
  where
    to = neighbour board c dir

-- | Checks if there is a unit in a given coordinate.
unitExists :: TileCoord -> TileMap -> Bool
unitExists c tMap = (isJust . tileUnit) (tMap ! c)

-- | Returns a list of all units and their coordinates.
allUnits :: TileMap -> [(TileCoord, Unit)]
allUnits = map (second fromJust)
           . filter (isJust . snd)
           . map (second tileUnit)
           . M.toList

-- | Returns a list of all cities and their coordinates.
allCities :: TileMap -> [(TileCoord, Improvement)]
allCities = filter ((== City) . snd)
            . map (second fromJust)
            . filter (isJust . snd)
            . map (second tileImprovement)
            . M.toList

-- | Checks if there are any units waiting to be played in this turn.
-- Returns if it finds one. If there is none, then it's time to end the turn.
findNextUnitInLine :: TileMap -> Maybe TileCoord
findNextUnitInLine tMap =
    case u'' of
      []      -> Nothing
      (c,_):_ -> Just c
  where
    units = allUnits tMap
    u'    = filter (unitInLine . snd) units
    u''   = filter ((/= 0) . unitMovementLeft . snd) u' -- checking again

-- | Change the current `nextUnitInLine`'s `unitInLine` to False
deactivateNextUnitInLine :: TileCoord -> TileMap -> TileMap
deactivateNextUnitInLine c tMap = M.insert c newTile tMap
  where
    tile = tMap ! c
    unitish = tileUnit tile :: Maybe Unit
    newTile = maybe tile
              (\u -> tile {tileUnit = Just $ u {unitInLine = False}}) unitish
