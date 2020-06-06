{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ShowSafe.Data
  ( ConKind (..),
    Prec,
    Renderer,
    --
    -- TODO : export from package only type
    --
    Salt (..),
    newPrec,
    newRen,
    appRen,
    newSalt,
  )
where

import ShowSafe.Import.External

data ConKind
  = ConRec
  | ConTup
  | ConPref
  | ConInf String
  deriving (Eq)

newtype Prec
  = Prec Int
  deriving (Eq, Ord, Num)

newtype Renderer a
  = Renderer {unRen :: Endo a}
  deriving (Semigroup, Monoid)

newtype Salt
  = Salt ByteString

newPrec :: Int -> Prec
newPrec = Prec

newRen :: (Monoid a) => a -> Renderer a
newRen x = Renderer $ Endo (x <>)

appRen :: Renderer a -> a -> a
appRen = appEndo . unRen

newSalt :: ByteString -> Salt
newSalt = Salt
