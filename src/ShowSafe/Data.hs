{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShowSafe.Data
  ( newRenderer,
    appRenderer,
    Renderer,
  )
where

import ShowSafe.Import.External

newtype Renderer = Renderer (Endo Text) deriving (Semigroup, Monoid)

newRenderer :: (Text -> Text) -> Renderer
newRenderer f = Renderer $ Endo f

appRenderer :: Renderer -> Text -> Text
appRenderer r = appEndo (coerce r)
