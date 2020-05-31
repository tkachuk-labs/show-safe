{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ShowSafe.Class
  ( ShowSafe (..),
  )
where

import ShowSafe.Import

--
-- Public classes
--

appPrec :: Prec
appPrec = newPrec 2

showsSafePrecDefault ::
  (Generic a, GenShowSafe (Rep a), Monoid t, IsString t) =>
  Prec ->
  a ->
  Renderer t
showsSafePrecDefault p = gssPrec Pref p . from

class ShowSafe a where
  showsSafePrec :: (Monoid t, IsString t) => Prec -> a -> Renderer t
  default showsSafePrec ::
    (Generic a, GenShowSafe (Rep a), Monoid t, IsString t) =>
    Prec ->
    a ->
    Renderer t
  showsSafePrec = showsSafePrecDefault

  showsSafe :: (Monoid t, IsString t) => a -> Renderer t
  showsSafe = showsSafePrec 0

  showSafe :: (Monoid t, IsString t) => a -> t
  showSafe x = appRen (showsSafe x) mempty

  showSafeList :: (Monoid t, IsString t) => [a] -> Renderer t
  showSafeList xs =
    newRen "["
      <> mconcat (intersperse renComma $ showsSafePrec 0 <$> xs)
      <> newRen "]"

--
-- Public instances
--

instance ShowSafe Bool

instance ShowSafe ()

instance (ShowSafe a, ShowSafe b) => ShowSafe (a, b)

instance (ShowSafe a, ShowSafe b, ShowSafe c) => ShowSafe (a, b, c)

instance ShowSafe a => ShowSafe [a] where
  showsSafePrec _ = showSafeList

instance (ShowSafe (f p), ShowSafe (g p)) => ShowSafe ((f :+: g) p)

instance (ShowSafe (f p), ShowSafe (g p)) => ShowSafe ((f :*: g) p)

instance ShowSafe (f (g p)) => ShowSafe ((f :.: g) p)

--
-- Private Generic classes and instances
--
class GenShowSafe f where
  isNullary :: f p -> Bool
  gssPrec :: (Monoid t, IsString t) => ConsKind -> Prec -> f p -> Renderer t

instance GenShowSafe V1 where
  isNullary = const True
  gssPrec _ _ _ = mempty

instance GenShowSafe U1 where
  isNullary = const True
  gssPrec _ _ _ = mempty

instance (ShowSafe a) => GenShowSafe (K1 i a) where
  isNullary = const False
  gssPrec _ p x = showsSafePrec p $ unK1 x

instance (GenShowSafe a, Constructor c) => GenShowSafe (M1 C c a) where
  isNullary = const False
  gssPrec _ n c@(M1 x) =
    case fixity of
      Prefix ->
        renParen
          (n > appPrec && not (isNullary x))
          ( newRen (fromString $ conName c)
              <> (if isNullary x then mempty else renSpace)
              <> renCon ck (gssPrec ck appPrec x)
          )
      Infix _ m ->
        let prec1 = newPrec m
         in renParen (n > prec1) (renCon ck $ gssPrec ck prec1 x)
    where
      fixity = conFixity c
      ck =
        if conIsRecord c
          then Rec
          else case conName c of
            ('(' : ',' : _) -> Tup
            _ -> case fixity of
              Prefix -> Pref
              Infix {} -> Inf (show (conName c)) -- is this show needed?

instance (Selector s, GenShowSafe a) => GenShowSafe (M1 S s a) where
  isNullary = isNullary . unM1
  gssPrec t n s@(M1 x)
    | selName s == "" = --showParen (n > appPrec)
      gssPrec t n x
    | otherwise =
      newRen (fromString $ selName s)
        <> newRen " = "
        <> gssPrec t 0 x

instance (GenShowSafe a) => GenShowSafe (M1 D d a) where
  isNullary = isNullary . unM1
  gssPrec t n = gssPrec t n . unM1

instance (GenShowSafe a, GenShowSafe b) => GenShowSafe (a :+: b) where
  isNullary (L1 x) = isNullary x
  isNullary (R1 x) = isNullary x
  gssPrec t n (L1 x) = gssPrec t n x
  gssPrec t n (R1 x) = gssPrec t n x

instance (GenShowSafe a, GenShowSafe b) => GenShowSafe (a :*: b) where
  isNullary = const False
  gssPrec t@Rec n (a :*: b) =
    gssPrec t n a <> newRen ", " <> gssPrec t n b
  gssPrec t@(Inf s) n (a :*: b) =
    gssPrec t n a <> newRen (fromString s) <> gssPrec t n b
  gssPrec t@Tup n (a :*: b) =
    gssPrec t n a <> renComma <> gssPrec t n b
  gssPrec t@Pref n (a :*: b) =
    gssPrec t (n + 1) a <> renSpace <> gssPrec t (n + 1) b
