module Textures where

import Graphics.Gloss.Rendering
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Juicy
import qualified Data.Map as M
import Control.Monad
import Control.Applicative
import Data.Maybe (fromJust)

type Texture = Picture
-- | A general map to access all textures whenever necessary.
type TextureMap = M.Map String Texture

textureMap :: IO TextureMap
textureMap = createTextureMap
    [ ("desert",  "assets/desert.png")
    , ("grass",   "assets/grass.png")
    , ("hill",    "assets/hill.png")
    , ("plains",  "assets/plains.png")
    , ("tundra",  "assets/tundra.png")
    , ("settler", "assets/settler.png")
    , ("worker",  "assets/worker.png")
    , ("city1",   "assets/city1.png")
    ]

-- | Create texture map from texture names and PNG file paths.
-- We don't really care if there are errors at this point.
-- We might choose to avoid partial functions in the future.
createTextureMap :: [(String, FilePath)] -> IO TextureMap
createTextureMap xs =
    M.fromList <$> mapM (\(x,p) -> (,) x <$> (fromJust <$> loadJuicyPNG p)) xs

from :: String -> TextureMap -> Texture
from = flip (M.!)
