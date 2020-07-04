module ShowSafe.Combinator
  ( appPrec,
    renCon,
    renParen,
    renComma,
    renSpace,
    renHash,
    renHashShow,
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

renHash ::
  (HashAlgorithm h, Show h, IsString s, Monoid s) =>
  ByteString ->
  h ->
  Maybe Salt ->
  Renderer s
renHash x h ms =
  newRen salted
    <> newRen (show h)
    <> renSpace
    <> newRen (show $ hashWith h $ s <> x)
  where
    s :: ByteString
    s = maybe mempty coerce ms
    salted = if isJust ms then "SALTED " else mempty

renHashShow ::
  (Show a, HashAlgorithm h, Show h, IsString s, Monoid s) =>
  a ->
  h ->
  Maybe Salt ->
  Renderer s
renHashShow = renHash . show
