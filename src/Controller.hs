--- Copyright 2022 The Australian National University, All rights reserved

module Controller where

import CodeWorld
import Model

import Data.Text (pack, unpack)
import Data.Maybe

-- | Compute the new Model in response to an Event.
handleEvent :: Event -> Model -> Model
handleEvent event (Model shapes tool colour) =
  case event of
    KeyPress key
      -- revert to an empty canvas
      | k == "Esc" -> startModel

      -- write the current model to the console
      | k == "D" -> trace (pack (show currentModel)) currentModel

      -- display the mystery image
      | k == "S" -> Model sample tool colour

      | k == "Backspace" || k == "Delete" -> Model (init shapes) tool colour

      | k == " " -> undefined  -- TODO

      | k == "T" -> Model shapes (nextTool tool) colour

      | k == "C" -> Model shapes tool (nextColour colour)

      | k == "+" || k == "=" -> case tool of
        RectangleTool s point -> Model shapes (RectangleTool (s + 0.1) point) colour
        _ -> currentModel

      | k == "-" || k == "_" -> case tool of
        RectangleTool s point -> Model shapes (RectangleTool (s - 0.1) point) colour
        _ -> currentModel

      -- ignore other keys
      | otherwise -> currentModel

      where
        k = unpack key

    PointerPress p -> case tool of
      LineTool _->  Model shapes (LineTool (Just p)) colour
      CircleTool _-> Model shapes (CircleTool (Just p)) colour
      TriangleTool _-> Model shapes (TriangleTool (Just p)) colour
      RectangleTool s _-> Model shapes (RectangleTool s (Just p)) colour
      _ -> currentModel


    PointerRelease p ->  case tool of
      LineTool p1        -> Model (shapes ++ [(Line (fromJust p1) p, colour)]) (LineTool Nothing) colour
      CircleTool p1      -> Model (shapes ++ [(Circle (fromJust p1) p, colour)]) (CircleTool Nothing) colour
      TriangleTool p1    -> Model (shapes ++ [(Triangle (fromJust p1) p, colour)]) (TriangleTool Nothing) colour
      RectangleTool s p1 -> Model (shapes ++ [(Rectangle s (fromJust p1) p, colour)]) (RectangleTool s Nothing) colour
      _                  -> currentModel

    -- ignore other events
    _ -> currentModel

    where
     currentModel = Model shapes tool colour

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
  RectangleTool _ Nothing -> CapTool Nothing Nothing
  CapTool Nothing Nothing -> LineTool Nothing
  _ -> tool
