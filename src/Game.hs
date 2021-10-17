module Game where

import qualified Data.Array as A
import qualified Data.Set as Set
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Interact

import BFS

windowDisplay :: Display
windowDisplay = InWindow "Window" (200, 200) (10, 10)

baseGrid :: Grid
baseGrid =
  (A.listArray ((0, 0), (3, 3)) (replicate 16 Empty))
  A.//
  [((1, 1), Wall), ((1, 2), Wall), ((2, 1), Wall)]


data World = World
  { playerLocation :: Location
  , startLocation :: Location
  , endLocation :: Location
  , worldGrid :: Grid
  }

data CellCoordinates = CellCoordinates
  { cellCenter :: Point
  , cellTopLeft :: Point
  , cellTopRight :: Point
  , cellBottomRight :: Point
  , cellBottomLeft :: Point
  }

main :: IO ()
main = play
  windowDisplay
  white
  1
  (World (0, 0) (0, 0) (2, 2) baseGrid)
  drawingFunc
  inputHandler
  updateFunc

drawingFunc :: World -> Picture
drawingFunc world = Pictures
  [ gridPic, startPic, endPic, playerMarker
  ]
  where
    conversion = locationToCoords (-75, 75) 50
    (CellCoordinates _ stl str sbr sbl) = conversion (startLocation world)
    startPic = Color blue (Polygon [stl, str, sbr, sbl])
    (CellCoordinates _ etl etr ebr ebl) = conversion (endLocation world)
    endPic = Color green (Polygon [etl, etr, ebr, ebl])
    (px, py) = cellCenter (conversion (playerLocation world))
    playerMarker = Color red (translate px py (Circle 10))
    walls = filter (\(_, w) -> w == Wall) (A.assocs $ worldGrid world)
    gridPic = Pictures (map mapPic walls)

    mapPic (loc, _) = let (CellCoordinates _ tl tr br bl) = conversion loc in Color black (Polygon [tl, tr, br, bl])

inputHandler :: Event -> World -> World
inputHandler _ w = w

updateFunc :: Float -> World -> World
updateFunc _ w@(World playerLoc _ endLoc grid) =
  case path of
    (first : rest) -> w {playerLocation = first}
    _ -> w
  where
    path = bfsSearch grid playerLoc endLoc

locationToCoords :: (Float, Float) -> Float -> Location -> CellCoordinates
locationToCoords (xOffset, yOffset) cellSize (x, y) = CellCoordinates
  (centerX, centerY)
  (centerX - halfCell, centerY + halfCell) -- Top Left
  (centerX + halfCell, centerY + halfCell) -- Top Right
  (centerX + halfCell, centerY - halfCell) -- Bottom Right
  (centerX - halfCell, centerY - halfCell) -- Bottom Left
  where
    (centerX, centerY) = (xOffset + (fromIntegral x) * cellSize, yOffset - (fromIntegral y) * cellSize)
    halfCell = cellSize / 2.0

