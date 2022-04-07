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
handleEvent event (Model shapes tool colour saves) =
  case event of
    KeyPress key
      -- revert to an empty canvas
      | k == "Esc" -> startModel

      -- write the current model to the console
      | k == "D" -> trace (pack (show currentModel)) currentModel

      -- display the mystery image
      | k == "S" -> Model sample tool colour saves

      | k == "Backspace" || k == "Delete" -> Model (init shapes) tool colour saves

      | k == " " -> case tool of
        PolygonTool ps -> Model (shapes ++ [(Polygon ps, colour)]) (PolygonTool []) colour saves
        _ -> currentModel

      | k == "T" -> Model shapes (nextTool tool) colour saves

      | k == "C" -> Model shapes tool (nextColour colour) saves

      | k == "A" -> Model shapes tool colour (saves ++ [shapes]) -- Add to save list

      | k == "Z" || not (null saves) -> Model (head saves) tool colour (init saves) -- Go back in the history of save list and then drop preceeding save

      | k == "+" || k == "=" -> case tool of
        RectangleTool s point -> Model shapes (RectangleTool (s + 0.1) point) colour saves
        GeneralPolygonTool v p -> Model shapes (GeneralPolygonTool (v + 1) p) colour saves
        _ -> currentModel

      | k == "-" || k == "_" -> case tool of
        RectangleTool s point | s > 1 -> Model shapes (RectangleTool (s - 0.1) point) colour saves
        GeneralPolygonTool v p | v > 3 -> Model shapes (GeneralPolygonTool (v - 1) p) colour saves
        _ -> currentModel

      -- ignore other keys
      | otherwise -> currentModel

      where
        k = unpack key

    PointerPress p -> case tool of
      LineTool _->  Model shapes (LineTool (Just p)) colour saves
      CircleTool _-> Model shapes (CircleTool (Just p)) colour saves
      TriangleTool _-> Model shapes (TriangleTool (Just p)) colour saves
      RectangleTool s _-> Model shapes (RectangleTool s (Just p)) colour saves
      CapTool Nothing Nothing -> Model shapes (CapTool (Just p) Nothing) colour saves
      GeneralPolygonTool v Nothing -> Model shapes (GeneralPolygonTool v (Just p)) colour saves
      _ -> currentModel


    PointerRelease p ->  case tool of
      PolygonTool ps -> Model shapes (PolygonTool (ps ++ [p])) colour saves
      LineTool p1        -> Model (shapes ++ [(Line (fromJust p1) p, colour)]) (LineTool Nothing) colour saves
      CircleTool p1      -> Model (shapes ++ [(Circle (fromJust p1) p, colour)]) (CircleTool Nothing) colour saves
      TriangleTool p1    -> Model (shapes ++ [(Triangle (fromJust p1) p, colour)]) (TriangleTool Nothing) colour saves
      RectangleTool s p1 -> Model (shapes ++ [(Rectangle s (fromJust p1) p, colour)]) (RectangleTool s Nothing) colour saves
      -- cap tool here has two cases, when the user finishes off the circle segment OR selects the cut off point
      GeneralPolygonTool v translationPoint -> Model (shapes ++ [(GeneralPolygon nonMaybePoint (generateGeneralPolygonPoints nonMaybePoint p v), colour)]) (GeneralPolygonTool v Nothing) colour saves
        where nonMaybePoint = fromJust translationPoint
      CapTool pp Nothing -> Model shapes (CapTool pp (Just p)) colour saves
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
  RectangleTool _ Nothing -> GeneralPolygonTool 3.0 Nothing
  GeneralPolygonTool _ Nothing -> CapTool Nothing Nothing
  CapTool Nothing Nothing -> LineTool Nothing
  _ -> tool

generateGeneralPolygonPoints :: Point -> Point -> Double -> [Point]
generateGeneralPolygonPoints translationPoint point vertices = [rotatedPoint ((nVertex * (360 / vertices)) * (pi/180)) relativePoint | nVertex <- [1..vertices]]
  where
    relativePoint = Data.Bifunctor.bimap (fst point -) (snd point -) translationPoint
