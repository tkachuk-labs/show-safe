{-# LANGUAGE ConstraintKinds #-}
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
-- Private class
--

class GSS f where
  isNullary :: f p -> Bool
  gssPrec :: (Monoid s, IsString s) => ConKind -> Prec -> f p -> Renderer s

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

instance (Constructor c, GSS a) => GSS (M1 C c a) where
  isNullary = const False
  gssPrec _ n c@(M1 x) =
    case fixity of
      Prefix ->
        let content = renCon ck (gssPrec ck appPrec x)
            conWrap =
              newRen (fromString cn)
                <> (if isNullary x then mempty else renSpace)
                <> content
            wrapped = if ck == ConTup then content else conWrap
         in renParen
              (n > appPrec && not (isNullary x))
              wrapped
      Infix _ m ->
        let prec1 = newPrec m
         in renParen
              (n > prec1)
              (renCon ck $ gssPrec ck prec1 x)
    where
      fixity = conFixity c
      cn = conName c
      ck =
        if conIsRecord c
          then ConRec
          else case cn of
            ('(' : ',' : _) -> ConTup
            _ -> case fixity of
              Prefix -> ConPref
              Infix {} -> ConInf cn

instance (Selector s, GSS a) => GSS (M1 S s a) where
  isNullary = isNullary . unM1
  gssPrec t n s@(M1 x) =
    if null sn
      then gssPrec t n x --showParen (n > appPrec)
      else
        newRen (fromString sn)
          <> newRen " = "
          <> gssPrec t 0 x
    where
      sn = selName s

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
  gssPrec t@ConRec n (a :*: b) =
    gssPrec t n a <> newRen ", " <> gssPrec t n b
  gssPrec t@(ConInf s) n (a :*: b) =
    gssPrec t n a <> newRen (fromString s) <> gssPrec t n b
  gssPrec t@ConTup n (a :*: b) =
    gssPrec t n a <> renComma <> gssPrec t n b
  gssPrec t@ConPref n (a :*: b) =
    gssPrec t (n + 1) a <> renSpace <> gssPrec t (n + 1) b

--
-- Public class
--

type SS a = (ShowSafe a)

class ShowSafe a where
  showsSafePrec :: (Monoid s, IsString s) => Prec -> a -> Renderer s
  default showsSafePrec ::
    (Generic a, GSS (Rep a), Monoid s, IsString s) =>
    Prec ->
    a ->
    Renderer s
  showsSafePrec p = gssPrec ConPref p . from

  showsSafe :: (Monoid s, IsString s) => a -> Renderer s
  showsSafe = showsSafePrec 0

  showSafe :: (Monoid s, IsString s) => a -> s
  showSafe x = appRen (showsSafe x) mempty

  showSafeList :: (Monoid s, IsString s) => [a] -> Renderer s
  showSafeList xs =
    newRen "["
      <> mconcat (intersperse renComma $ showsSafePrec 0 <$> xs)
      <> newRen "]"

--
-- Public instances
--

instance ShowSafe ()

instance
  (SS a, SS b) =>
  ShowSafe (a, b)

instance
  (SS a, SS b, SS c) =>
  ShowSafe (a, b, c)

instance
  (SS a, SS b, SS c, SS d) =>
  ShowSafe (a, b, c, d)

instance
  (SS a, SS b, SS c, SS d, SS e) =>
  ShowSafe (a, b, c, d, e)

instance
  (SS a, SS b, SS c, SS d, SS e, SS f) =>
  ShowSafe (a, b, c, d, e, f)

instance
  (SS a, SS b, SS c, SS d, SS e, SS f, SS g) =>
  ShowSafe (a, b, c, d, e, f, g)

instance SS a => ShowSafe [a] where
  showsSafePrec _ = showSafeList

instance ShowSafe Bool

instance ShowSafe Any

instance ShowSafe All
