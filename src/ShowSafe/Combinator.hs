module ShowSafe.Combinator (renCon, renParen, renComma, renSpace) where

import ShowSafe.Data
import ShowSafe.Import.External

renCon :: (Monoid s, IsString s) => ConsKind -> Renderer s -> Renderer s
renCon Rec p = newRen "{" <> p <> newRen "}"
renCon Tup p = newRen "(" <> p <> newRen ")"
renCon Pref p = p
renCon Inf {} p = p

renParen :: (Monoid s, IsString s) => Bool -> Renderer s -> Renderer s
renParen c x =
  if c
    then newRen "(" <> x <> newRen ")"
    else x

renComma :: (Monoid s, IsString s) => Renderer s
renComma = newRen ","

renSpace :: (Monoid s, IsString s) => Renderer s
renSpace = newRen " "
