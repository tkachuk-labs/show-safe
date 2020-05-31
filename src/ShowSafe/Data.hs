{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShowSafe.Data
  ( ConsKind (..),
    Renderer,
    newRen,
    parenRen,
    appRen,
  )
where

import ShowSafe.Import.External

data ConsKind = Rec | Tup | Pref | Inf String

newtype Renderer a = Renderer (Endo a) deriving (Semigroup, Monoid)

--
-- TODO : unRen
--

newRen :: (Monoid a) => a -> Renderer a
newRen x = Renderer $ Endo (x <>)

appRen :: Renderer a -> a -> a
appRen (Renderer x) = appEndo x

-- TODO : unRen

parenRen :: (Monoid a, IsString a) => Bool -> Renderer a -> Renderer a
parenRen c x =
  if c
    then newRen "(" <> x <> newRen ")"
    else x
