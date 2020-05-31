{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShowSafe.Data
  ( ConsKind (..),
    Renderer,
    newRen,
    appRen,
  )
where

import ShowSafe.Import.External

data ConsKind = Rec | Tup | Pref | Inf Text

newtype Renderer = Renderer (Endo Text) deriving (Semigroup, Monoid)

newRen :: Text -> Renderer
newRen x = Renderer $ Endo (x <>)

appRen :: Renderer -> Text -> Text
appRen r = appEndo (coerce r)
