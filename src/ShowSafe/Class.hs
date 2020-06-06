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
  gssPrec ::
    (HashAlgorithm h, Monoid s, IsString s) =>
    h ->
    Maybe Salt ->
    ConKind ->
    Prec ->
    f p ->
    Renderer s

--
-- Private instances
--

instance GSS V1 where
  isNullary = const True
  gssPrec _ _ _ _ _ = mempty

instance GSS U1 where
  isNullary = const True
  gssPrec _ _ _ _ _ = mempty

instance (ShowSafe a) => GSS (K1 i a) where
  isNullary = const False
  gssPrec h s _ p x = showsSafePrec h s p $ unK1 x

instance (Constructor c, GSS a) => GSS (M1 C c a) where
  isNullary = const False
  gssPrec h s _ n c@(M1 x) =
    case fixity of
      Prefix ->
        let content = renCon ck (gssPrec h s ck appPrec x)
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
              (renCon ck $ gssPrec h s ck prec1 x)
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
  gssPrec h salt t n s@(M1 x) =
    if null sn
      then gssPrec h salt t n x --showParen (n > appPrec)
      else
        newRen (fromString sn)
          <> newRen " = "
          <> gssPrec h salt t 0 x
    where
      sn = selName s

instance (GSS a) => GSS (M1 D d a) where
  isNullary = isNullary . unM1
  gssPrec h s t n = gssPrec h s t n . unM1

instance (GSS a, GSS b) => GSS (a :+: b) where
  isNullary (L1 x) = isNullary x
  isNullary (R1 x) = isNullary x
  gssPrec h s t n (L1 x) = gssPrec h s t n x
  gssPrec h s t n (R1 x) = gssPrec h s t n x

instance (GSS a, GSS b) => GSS (a :*: b) where
  isNullary = const False
  gssPrec h s t@ConRec n (a :*: b) =
    gssPrec h s t n a <> newRen ", " <> gssPrec h s t n b
  gssPrec h s t@(ConInf c) n (a :*: b) =
    gssPrec h s t n a <> newRen (fromString c) <> gssPrec h s t n b
  gssPrec h s t@ConTup n (a :*: b) =
    gssPrec h s t n a <> renComma <> gssPrec h s t n b
  gssPrec h s t@ConPref n (a :*: b) =
    gssPrec h s t (n + 1) a <> renSpace <> gssPrec h s t (n + 1) b

--
-- Public class
--

type SS a = (ShowSafe a)

class ShowSafe a where
  showsSafePrec ::
    (HashAlgorithm h, Monoid s, IsString s) =>
    h ->
    Maybe Salt ->
    Prec ->
    a ->
    Renderer s
  default showsSafePrec ::
    (HashAlgorithm h, Generic a, GSS (Rep a), Monoid s, IsString s) =>
    h ->
    Maybe Salt ->
    Prec ->
    a ->
    Renderer s
  showsSafePrec h s p =
    gssPrec h s ConPref p . from

  showsSafe ::
    (HashAlgorithm h, Monoid s, IsString s) =>
    h ->
    Maybe Salt ->
    a ->
    Renderer s
  showsSafe alg salt =
    showsSafePrec alg salt 0

  showSafe ::
    (HashAlgorithm h, Monoid s, IsString s) =>
    h ->
    Maybe Salt ->
    a ->
    s
  showSafe alg salt x =
    appRen (showsSafe alg salt x) mempty

  showSafeList ::
    (HashAlgorithm h, Monoid s, IsString s) =>
    h ->
    Maybe Salt ->
    [a] ->
    Renderer s
  showSafeList alg salt xs =
    newRen "["
      <> mconcat (intersperse renComma $ showsSafePrec alg salt 0 <$> xs)
      <> newRen "]"

--
-- Public instances
--

--
-- TODO : implement all instances from GHC.Show
--

instance ShowSafe Bool

--
-- TODO : remove me
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
  showsSafePrec alg salt _ = showSafeList alg salt

instance ShowSafe Any

instance ShowSafe All
