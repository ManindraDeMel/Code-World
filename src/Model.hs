--- Copyright 2022 The Australian National University, All rights reserved

module Model where

import CodeWorld

data Shape
  -- | The Line constructor holds the start and end coordinates of the line.
  = Line Point Point
  -- | For the Polygon, a list of vertices.
  | Polygon [Point]
  -- | For the Circle, the centre and a point on the circumference.
  | Circle Point Point
  -- | For the (isosceles) Triangle,
  --   coordinates of the apex and one of the base vertices. 
  | Triangle Point Point
  -- | For the Rectangle,
  --   the scaling factor, and coordinates of two connected vertices.
  | Rectangle Double Point Point
  -- | For the Cap, the centre and a circumference point of
  --   the defining circle, then the y level of the cap.
  | Cap Point Point Double

  | GeneralPolygon Point [Point] 
  deriving (Show)

type ColourShape = (Shape, ColourName)

data Tool
  = LineTool (Maybe Point)
  | PolygonTool [Point]
  | CircleTool (Maybe Point)
  | TriangleTool (Maybe Point)
  | RectangleTool Double (Maybe Point)
  | CapTool (Maybe Point) (Maybe Point)
  | GeneralPolygonTool Double (Maybe Point)
  deriving (Eq, Show)

data ColourName
  = Black
  | Red
  | Orange
  | Yellow
  | Green
  | Blue
  | Purple
  | White
  deriving (Eq, Show, Enum)

data Model = Model [ColourShape] Tool ColourName [[ColourShape]]
  deriving (Show)

-- | Starting Model for when CodeWorld first starts up.
startModel :: Model
startModel = Model [] (LineTool Nothing) Black []

-- | A sample image.
sample :: [ColourShape]
sample =
  [
    (Polygon [(2.8,-2.5),(3.5,-3.5),(3.3,-4),(2.8,-3.9),(2.1,-3)],Yellow),
    (Polygon [(-0.8,-2.5),(-1.5,-3.5),(-1.3,-4),(-0.8,-3.9),(-0.1,-3)],Yellow),
    (Line (1.9,-1.9) (2.7,-0.2),Black),
    (Line (1.4,-1.6) (1.9,-1.9),Black),
    (Line (2.1,0) (1.4,-1.6),Black),
    (Line (0.1,-1.9) (-0.7,-0.2),Black),
    (Line (0.6,-1.6) (0.1,-1.9),Black),
    (Line (-0.1,0) (0.6,-1.6),Black),
    (Circle (1,-0.7) (1,2),Yellow),
    (Rectangle 0.3 (-2,-0.4) (-4.8,0.8),Yellow),
    (Rectangle 2 (-1.4,-1) (-1.6,-1.5),Orange),
    (Rectangle 4 (-2.2,-1.25) (-2.6,-1.15),Yellow),
    (Triangle (1,5.3) (0,5.8),Blue),
    (Cap (1,6) (1,7) 5.8,Blue),
    (Triangle (2,7) (1.9,6.5),Black),
    (Triangle (0,7) (-0.1,6.5),Black),
    (Triangle (2,7) (2.3,5.5),Yellow),
    (Triangle (0,7) (-0.3,5.5),Yellow),
    (Circle (1.5,4.8) (1.5,4.5),Black),
    (Circle (0.5,4.8) (0.5,4.5),Black),
    (Line (1.5,2.9) (1.7,3.3),Black),
    (Line (0.5,2.9) (1.5,2.9),Black),
    (Line (0.3,3.3) (0.5,2.9),Black),
    (Circle (2.5,4) (2.5,3.7),Red),
    (Circle (-0.5,4) (-0.5,3.7),Red),
    (Circle (1,4) (1,6),Yellow)
  ]