--- Copyright 2022 The Australian National University, All rights reserved

module View where

import CodeWorld
import Data.Text (pack)
import Model

-- | Render all the parts of a Model to a CodeWorld picture.
-- | You do not need to understand all parts of this function.
modelToPicture :: Model -> Picture
modelToPicture (Model ss t c)
  = translated 0 8 toolText
  & translated 0 7 colourText
  & translated 0 (-8) areaText
  & colourShapesToPicture ss
  & coordinatePlane
  where
    colourText = stringToText (show c)
    toolText = stringToText (toolToLabel t)
    areaText = stringToText (case t of
      RectangleTool r _ -> "Current scaling factor: " ++
        takeWhile (/='.') (show r) ++ take 2 (dropWhile (/='.') (show r))
      _ -> "")
    stringToText = lettering . pack

-- TODO
toolToLabel :: Tool -> String
toolToLabel tool = case tool of 
  LineTool _ -> "Line: click-drag-release"
  PolygonTool _ -> "Polygon: click 3 or more times then spacebar"
  CircleTool _ -> "Circle: click-drag-release between centre and circumference"
  TriangleTool _ -> "Triangle: click-drag release for first 2 corners"
  RectangleTool _ _ -> "Rectangle: +/- to increase/decrease scaling factor; click-drag release for first 2 corners"
  CapTool _ _ -> "Cap: click-drag-release for circle, then click for cap level"

-- TODO
colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture = undefined

-- TODO
colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture = undefined

-- TODO
colourNameToColour :: ColourName -> Colour
colourNameToColour = undefined

-- TODO
shapeToPicture :: Shape -> Picture
shapeToPicture = undefined