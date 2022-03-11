--- Copyright 2022 The Australian National University, All rights reserved

module Controller where

import CodeWorld
import Model

import Data.Text (pack, unpack)

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

      | k == "Backspace" || k == "Delete" -> undefined  -- TODO

      | k == " " -> undefined  -- TODO
   
      | k == "T" -> undefined  -- TODO

      | k == "C" -> undefined  -- TODO

      | k == "+" || k == "=" -> undefined  -- TODO

      | k == "-" || k == "_" -> undefined  -- TODO

      -- ignore other keys
      | otherwise -> currentModel
      
      where
        k = unpack key

    PointerPress p -> undefined  -- TODO

    PointerRelease p -> undefined  -- TODO
    
    -- ignore other events
    _ -> currentModel

    where
     currentModel = Model shapes tool colour

-- TODO
nextColour :: ColourName -> ColourName
nextColour = undefined

-- TODO
nextTool :: Tool -> Tool
nextTool = undefined