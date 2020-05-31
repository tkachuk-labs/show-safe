module ShowSafe.Combinator
  ( appPrec,
    renCon,
    renParen,
    renComma,
    renSpace,
  )
where

import qualified GHC.Show as S (appPrec)
import ShowSafe.Data
import ShowSafe.Import.External

appPrec :: Prec
appPrec = newPrec S.appPrec

renCon :: (Monoid s, IsString s) => ConKind -> Renderer s -> Renderer s
renCon ConRec p = newRen "{" <> p <> newRen "}"
renCon ConTup p = newRen "(" <> p <> newRen ")"
renCon ConPref p = p
renCon ConInf {} p = p

renParen :: (Monoid s, IsString s) => Bool -> Renderer s -> Renderer s
renParen c x =
  if c
    then newRen "(" <> x <> newRen ")"
    else x

renComma :: (Monoid s, IsString s) => Renderer s
renComma = newRen ","

renSpace :: (Monoid s, IsString s) => Renderer s
renSpace = newRen " "
