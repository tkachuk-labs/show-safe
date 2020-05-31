{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ShowSafe.Class
  ( ShowSafe (..),
  )
where

import ShowSafe.Import

--
-- Private class
--

class GSS f where
  isNullary :: f p -> Bool
  gssPrec :: (Monoid t, IsString t) => ConsKind -> Prec -> f p -> Renderer t

--
-- Private instances
--

instance GSS V1 where
  isNullary = const True
  gssPrec _ _ _ = mempty

instance GSS U1 where
  isNullary = const True
  gssPrec _ _ _ = mempty

instance (ShowSafe a) => GSS (K1 i a) where
  isNullary = const False
  gssPrec _ p x = showsSafePrec p $ unK1 x

instance (GSS a, Constructor c) => GSS (M1 C c a) where
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

instance (Selector s, GSS a) => GSS (M1 S s a) where
  isNullary = isNullary . unM1
  gssPrec t n s@(M1 x)
    | selName s == "" = --showParen (n > appPrec)
      gssPrec t n x
    | otherwise =
      newRen (fromString $ selName s)
        <> newRen " = "
        <> gssPrec t 0 x

instance (GSS a) => GSS (M1 D d a) where
  isNullary = isNullary . unM1
  gssPrec t n = gssPrec t n . unM1

instance (GSS a, GSS b) => GSS (a :+: b) where
  isNullary (L1 x) = isNullary x
  isNullary (R1 x) = isNullary x
  gssPrec t n (L1 x) = gssPrec t n x
  gssPrec t n (R1 x) = gssPrec t n x

instance (GSS a, GSS b) => GSS (a :*: b) where
  isNullary = const False
  gssPrec t@Rec n (a :*: b) =
    gssPrec t n a <> newRen ", " <> gssPrec t n b
  gssPrec t@(Inf s) n (a :*: b) =
    gssPrec t n a <> newRen (fromString s) <> gssPrec t n b
  gssPrec t@Tup n (a :*: b) =
    gssPrec t n a <> renComma <> gssPrec t n b
  gssPrec t@Pref n (a :*: b) =
    gssPrec t (n + 1) a <> renSpace <> gssPrec t (n + 1) b

--instance GSS UChar where
--  isNullary = const False
--  gssPrec _ _ (UChar c) = showsSafePrec (newPrec 0) (C# c) <> newRen "#"
--
--instance GSS UDouble where
--  isNullary = const False
--  gssPrec _ _ (UDouble d) = showsSafePrec (newPrec 0) (D# d) <> newRen "##"
--
--instance GSS UFloat where
--  isNullary = const False
--  gssPrec _ _ (UFloat f) = showsSafePrec (newPrec 0) (F# f) <> newRen "#"
--
--instance GSS UInt where
--  isNullary = const False
--  gssPrec _ _ (UInt i) = showsSafePrec (newPrec 0) (I# i) <> newRen "#"
--
--instance GSS UWord where
--  isNullary = const False
--  gssPrec _ _ (UWord w) = showsSafePrec (newPrec 0) (W# w) <> (newRen "##")

--
-- Public class
--

type SS a = (ShowSafe a)

class ShowSafe a where
  showsSafePrec :: (Monoid t, IsString t) => Prec -> a -> Renderer t
  default showsSafePrec ::
    (Generic a, GSS (Rep a), Monoid t, IsString t) =>
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

appPrec :: Prec
appPrec = newPrec 2

showsSafePrecDefault ::
  (Generic a, GSS (Rep a), Monoid t, IsString t) =>
  Prec ->
  a ->
  Renderer t
showsSafePrecDefault p = gssPrec Pref p . from

--
-- Public instances
--

instance ShowSafe Bool

instance ShowSafe ()

instance (SS a, SS b) => ShowSafe (a, b)

instance (SS a, SS b, SS c) => ShowSafe (a, b, c)

instance ShowSafe a => ShowSafe [a] where
  showsSafePrec _ = showSafeList

instance (ShowSafe (f p), ShowSafe (g p)) => ShowSafe ((f :+: g) p)

instance (ShowSafe (f p), ShowSafe (g p)) => ShowSafe ((f :*: g) p)

instance ShowSafe (f (g p)) => ShowSafe ((f :.: g) p)
