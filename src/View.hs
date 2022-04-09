--- Copyright 2022 The Australian National University, All rights reserved

module View where

import CodeWorld
import Data.Text (pack)
import Model

-- | Render all the parts of a Model to a CodeWorld picture.
-- | You do not need to understand all parts of this function.
modelToPicture :: Model -> Picture
modelToPicture (Model ss t c sl)
  = translated 0 8 toolText
  & translated 0 7 colourText
  & translated 0 (-8) areaText
  & translated (-16) 0 saveText
  & translated (-16) (-1) saveText2 -- the text placement might be differing on different dimension monitors
  & translated (-16) (-2) saveText3
  & colourShapesToPicture ss
  & coordinatePlane
  where
    saveText = stringToText $ "Saves:" ++ show (length sl)
    saveText2 = stringToText "a: save" 
    saveText3= stringToText "z: previous save" 
    colourText = stringToText (show c)
    toolText = stringToText (toolToLabel t)
    areaText = stringToText (case t of
      RectangleTool r _ -> "Current scaling factor: " ++
        takeWhile (/='.') (show r) ++ take 2 (dropWhile (/='.') (show r))
      GeneralPolygonTool v _ -> "Current vertex count: " ++
        takeWhile (/='.') (show v) ++ take 2 (dropWhile (/='.') (show v))
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
  GeneralPolygonTool _ _ -> "General Polygon: +/- to increase/decrease scaling factor; click-drag-release for centre to 1 vertex"
  CapTool _ _ -> "Cap: click-drag-release for circle, then click for cap level"


-- 
colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture colourshapeList = pictures $ map colourShapeToPicture colourshapeList

-- 
colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture (shape, color) = coloured (colourNameToColour color) (shapeToPicture shape)

-- 
colourNameToColour :: ColourName -> Colour
colourNameToColour colourName = case colourName of
  Black -> black
  Red -> red
  Orange -> orange
  Yellow -> yellow
  Green -> green
  Blue -> blue
  Purple -> purple
  White -> white

-- TODO
shapeToPicture :: Shape -> Picture
shapeToPicture shape = case shape of
  Line a b -> polyline [a, b]
  Polygon points -> solidPolygon points
  Circle (a, b) (c, d) -> translated a b $ solidCircle (sqrt((a - c)**2 + (b - d)**2))
  Triangle (a, b) (c, d) -> solidPolygon [(a, b), (2*a - c, d), (c, d)] 

  Rectangle s (a, b) (c, d) -> translated ((a + c) / 2) ((b + d) / 2) $ rotated angle $ solidRectangle rectLength (s * rectLength)
    where
      rectLength = sqrt $ (a - c) ** 2 + (b - d) ** 2
      angle = atan2 (b-d) (a-c)

  Cap (a, b) (c, d) cutoff -> translateSemiCircle rectangleClip
    where
      radius = sqrt $ (a - c)**2 + (b - d)**2
      translateSemiCircle = translated a (b + radius) -- here we created move the semi circle to the desired centre
                      -- Here we create the dimensions of the rectangle to cut off the circle       -- then translate it up to the centre 
      rectangleClip = clipped (2 * radius) (2 * (b - cutoff + radius)) (translated 0 (-radius) $ solidCircle radius)

  GeneralPolygon (a, b) points -> translated a b $ solidPolygon points -- translate the polygon back to the centre the user defined


