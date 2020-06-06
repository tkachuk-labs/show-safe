module ShowSafe.Combinator
  ( showHash,
    appPrec,
    renCon,
    renParen,
    renComma,
    renSpace,
  )
where

import qualified GHC.Show as S (appPrec)
import ShowSafe.Data
import ShowSafe.Import.External

showHash ::
  (HashAlgorithm h, Show h, Show a, IsString b, Monoid b) =>
  h ->
  Maybe Salt ->
  a ->
  b
showHash h ms x =
  show h <> salted <> show (hashWith h $ s <> show x)
  where
    s :: ByteString
    s = maybe mempty coerce ms
    salted = if isJust ms then " SALTED " else " "

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
