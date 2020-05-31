{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShowSafe.Data
  ( ConsKind (..),
    Prec,
    Renderer,
    newPrec,
    newRen,
    parenRen,
    appRen,
  )
where

import ShowSafe.Import.External

data ConsKind = Rec | Tup | Pref | Inf String

newtype Prec = Prec Int deriving (Eq, Ord, Num)

newtype Renderer a
  = Renderer {unRen :: Endo a}
  deriving (Semigroup, Monoid)

newPrec :: Int -> Prec
newPrec = Prec

newRen :: (Monoid a) => a -> Renderer a
newRen x = Renderer $ Endo (x <>)

appRen :: Renderer a -> a -> a
appRen = appEndo . unRen

-- TODO : unRen

parenRen :: (Monoid a, IsString a) => Bool -> Renderer a -> Renderer a
parenRen c x =
  if c
    then newRen "(" <> x <> newRen ")"
    else x
