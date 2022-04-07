--- Copyright 2022 The Australian National University, All rights reserved
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant where" #-}

module Controller where

import CodeWorld
import Model

import Data.Text (pack, unpack)
import Data.Maybe

import Data.Bifunctor

-- | Compute the new Model in response to an Event.
handleEvent :: Event -> Model -> Model
handleEvent event (Model shapes tool colour saves) = -- (the saves list is 4.1 in COMP1130 Extensions)
  case event of
    KeyPress key
      -- revert to an empty canvas
      | k == "Esc" -> startModel

      -- write the current model to the console
      | k == "D" -> trace (pack (show currentModel)) currentModel

      -- display the mystery image
      | k == "S" -> Model sample tool colour saves

      | (k == "Backspace" || k == "Delete") && not (null shapes)  -> Model (init shapes) tool colour saves -- (init shapes) drops the last element of the list which is the most recent shape in our case

      | k == " " -> case tool of
        PolygonTool ps -> Model (shapes ++ [(Polygon ps, colour)]) (PolygonTool []) colour saves -- This is like pointerRelease but for polygonTool
        _ -> currentModel

      | k == "T" -> Model shapes (nextTool tool) colour saves -- change tool

      | k == "C" -> Model shapes tool (nextColour colour) saves -- change colour

      | k == "A" -> Model shapes tool colour (saves ++ [shapes]) -- Add to save list, Notice that there are guards preventing from going back to a save that doesn't exist

      | k == "Z" && not (null saves) -> Model (head saves) tool colour (init saves) -- Go back in the history of save list and then drop preceding save

      | k == "+" || k == "=" -> case tool of
        RectangleTool s point -> Model shapes (RectangleTool (s + 0.1) point) colour saves -- add to the scale factor of a rectangle, notice there are guards to prevent negative scale factors
        GeneralPolygonTool v p -> Model shapes (GeneralPolygonTool (v + 1) p) colour saves -- add a vertex to the GeneralPolygonTool, notice there are guards to prevent negative scale factors
        _ -> currentModel

      | k == "-" || k == "_" -> case tool of
        RectangleTool s point | s > 1 -> Model shapes (RectangleTool (s - 0.1) point) colour saves -- decrease the scale factor of a rectangle, notice there are guards to prevent vertices below that of a triangle
        GeneralPolygonTool v p | v > 3 -> Model shapes (GeneralPolygonTool (v - 1) p) colour saves -- decrease the vertices of the GeneralPolygonTool, notice there are guards to prevent vertices below that of a triangle
        _ -> currentModel

      -- ignore other keys
      | otherwise -> currentModel

      where
        k = unpack key

    PointerPress p -> case tool of -- In Pointer press this is when the user clicks down on the mouse but hasn't released yet, this is where all the tools are initialized
      LineTool _->  Model shapes (LineTool (Just p)) colour saves 
      CircleTool _-> Model shapes (CircleTool (Just p)) colour saves
      TriangleTool _-> Model shapes (TriangleTool (Just p)) colour saves
      RectangleTool s _-> Model shapes (RectangleTool s (Just p)) colour saves
      CapTool Nothing Nothing -> Model shapes (CapTool (Just p) Nothing) colour saves
      GeneralPolygonTool v Nothing -> Model shapes (GeneralPolygonTool v (Just p)) colour saves
      _ -> currentModel


    PointerRelease p ->  case tool of -- Pointer release is the point where the user releases the mouse button. Usually this is the final point of a shape (edge of rectangle, edge of circle)
      PolygonTool ps -> Model shapes (PolygonTool (ps ++ [p])) colour saves
      LineTool p1        -> Model (shapes ++ [(Line (fromJust p1) p, colour)]) (LineTool Nothing) colour saves
      CircleTool p1      -> Model (shapes ++ [(Circle (fromJust p1) p, colour)]) (CircleTool Nothing) colour saves
      TriangleTool p1    -> Model (shapes ++ [(Triangle (fromJust p1) p, colour)]) (TriangleTool Nothing) colour saves
      RectangleTool s p1 -> Model (shapes ++ [(Rectangle s (fromJust p1) p, colour)]) (RectangleTool s Nothing) colour saves
      -- General polygon expects a list of points which we general with a custom function
      GeneralPolygonTool v translationPoint -> Model (shapes ++ [(GeneralPolygon nonMaybePoint (generateGeneralPolygonPoints nonMaybePoint p v), colour)]) (GeneralPolygonTool v Nothing) colour saves
        where nonMaybePoint = fromJust translationPoint
      -- Cap tool here has two cases, when the user finishes off the circle segment OR selects the cut off point
      CapTool pp Nothing -> Model shapes (CapTool pp (Just p)) colour saves
      -- Choosing cap point case
      CapTool a b -> Model (shapes ++ [(Cap p1 p2 p3, colour)]) (CapTool Nothing Nothing) colour saves
        where
          p1 = fromJust a
          p2 = fromJust b
          p3 = snd p


    -- ignore other events
    _ -> currentModel

    where
     currentModel = Model shapes tool colour saves

-- TODO
nextColour :: ColourName -> ColourName
nextColour colour = case colour of
  White -> Black
  _ -> succ colour

-- TODO
nextTool :: Tool -> Tool
nextTool tool = case tool of
  LineTool Nothing -> PolygonTool []
  PolygonTool [] -> CircleTool Nothing
  CircleTool Nothing -> TriangleTool Nothing
  TriangleTool Nothing -> RectangleTool 1.0 Nothing
  RectangleTool _ Nothing -> GeneralPolygonTool 3.0 Nothing -- A triangle is the simplest polygon and thus the vertex count is limited to 3
  GeneralPolygonTool _ Nothing -> CapTool Nothing Nothing -- 4.4 Extension for COMP1130
  CapTool Nothing Nothing -> LineTool Nothing
  _ -> tool

-- this function generates the rotated points for the General polygon (-- 4.4 Extension for COMP1130)
-- Effectivley the vertex point is translated back to rotate around the origin (vertex point - centre point). We then apply a property of polygons (360/number of vertices) is the 
-- angle between the vertices. Thus using list comphrehension we can achieve all the rotated points using Codeworlds in-built rotatedPoint function. 
-- Furthermore, in View.hs, shapeToPicture then translates this shape back to the centre the user defined. 
generateGeneralPolygonPoints :: Point -> Point -> Double -> [Point] 
generateGeneralPolygonPoints translationPoint point vertices = [rotatedPoint ((nVertex * (360 / vertices)) * (pi/180)) relativePoint | nVertex <- [1..vertices]]
  where
    relativePoint = Data.Bifunctor.bimap (fst point -) (snd point -) translationPoint -- Move the point to be rotated to be around the origin
