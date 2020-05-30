module ShowSafe.Data
  ( Renderer (..),
  )
where

import ShowSafe.Import.External

newtype Renderer = Renderer (Text -> Text)
