module ShowSafe.Combinator (renCon, renParen, renComma, renSpace) where

import ShowSafe.Data
import ShowSafe.Import.External

renCon :: (Monoid t, IsString t) => ConsKind -> Renderer t -> Renderer t
renCon Rec p = newRen "{" <> p <> newRen "}"
renCon Tup p = newRen "(" <> p <> newRen ")"
renCon Pref p = p
renCon Inf {} p = p

renParen :: (Monoid a, IsString a) => Bool -> Renderer a -> Renderer a
renParen c x =
  if c
    then newRen "(" <> x <> newRen ")"
    else x

renComma :: (Monoid a, IsString a) => Renderer a
renComma = newRen ","

renSpace :: (Monoid a, IsString a) => Renderer a
renSpace = newRen " "
