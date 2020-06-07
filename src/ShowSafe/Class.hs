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
  isNullary :: f a -> Bool
  gssPrec ::
    (HashAlgorithm h, Show h, Monoid s, IsString s) =>
    f a ->
    ConKind ->
    Prec ->
    h ->
    Maybe Salt ->
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
  gssPrec x _ = showsSafePrec (unK1 x)

instance (Constructor con, GSS a) => GSS (M1 C con a) where
  isNullary = const False
  gssPrec con@(M1 x) _ p h s =
    case fixity of
      Prefix ->
        let content = renCon t (gssPrec x t appPrec h s)
            conWrap =
              newRen (fromString cn)
                <> (if isNullary x then mempty else renSpace)
                <> content
            wrapped = if t == ConTup then content else conWrap
         in renParen
              (p > appPrec && not (isNullary x))
              wrapped
      Infix _ m ->
        let prec1 = newPrec m
         in renParen
              (p > prec1)
              (renCon t $ gssPrec x t prec1 h s)
    where
      fixity = conFixity con
      cn = conName con
      t =
        if conIsRecord con
          then ConRec
          else case cn of
            ('(' : ',' : _) -> ConTup
            _ -> case fixity of
              Prefix -> ConPref
              Infix {} -> ConInf cn

instance (Selector sel, GSS a) => GSS (M1 S sel a) where
  isNullary = isNullary . unM1
  gssPrec sel@(M1 x) t p h s =
    if null sn
      then gssPrec x t p h s
      else
        newRen (fromString sn)
          <> newRen " = "
          <> gssPrec x t 0 h s
    where
      sn = selName sel

instance (GSS a) => GSS (M1 D dat a) where
  isNullary = isNullary . unM1
  gssPrec = gssPrec . unM1

instance (GSS a, GSS b) => GSS (a :+: b) where
  isNullary (L1 x) = isNullary x
  isNullary (R1 x) = isNullary x
  gssPrec (L1 x) = gssPrec x
  gssPrec (R1 x) = gssPrec x

instance (GSS a, GSS b) => GSS (a :*: b) where
  isNullary = const False
  gssPrec (x :*: y) t@ConRec p h s =
    gssPrec x t p h s <> newRen ", " <> gssPrec y t p h s
  gssPrec (x :*: y) t@(ConInf c) p h s =
    gssPrec x t p h s <> newRen (fromString c) <> gssPrec y t p h s
  gssPrec (x :*: y) t@ConTup p h s =
    gssPrec x t p h s <> renComma <> gssPrec y t p h s
  gssPrec (x :*: y) t@ConPref p h s =
    gssPrec x t (p + 1) h s <> renSpace <> gssPrec y t (p + 1) h s

--
-- Public class
--

type SS a = (ShowSafe a)

class ShowSafe a where
  showsSafePrec ::
    (HashAlgorithm h, Show h, Monoid s, IsString s) =>
    a ->
    Prec ->
    h ->
    Maybe Salt ->
    Renderer s
  default showsSafePrec ::
    (HashAlgorithm h, Show h, Generic a, GSS (Rep a), Monoid s, IsString s) =>
    a ->
    Prec ->
    h ->
    Maybe Salt ->
    Renderer s
  showsSafePrec x =
    gssPrec (from x) ConPref

  showsSafe ::
    (HashAlgorithm h, Show h, Monoid s, IsString s) =>
    a ->
    h ->
    Maybe Salt ->
    Renderer s
  showsSafe x =
    showsSafePrec x 0

  showSafe ::
    (HashAlgorithm h, Show h, Monoid s, IsString s) =>
    a ->
    h ->
    Maybe Salt ->
    s
  showSafe x h s =
    appRen (showsSafe x h s) mempty

  showSafeList ::
    (HashAlgorithm h, Show h, Monoid s, IsString s) =>
    [a] ->
    h ->
    Maybe Salt ->
    Renderer s
  showSafeList xs h s =
    newRen "["
      <> mconcat (intersperse renComma $ (\x -> showsSafePrec x 0 h s) <$> xs)
      <> newRen "]"

--
-- Public instances
--

--
-- TODO : implement all instances from GHC.Show
--

instance ShowSafe Bool

instance SS a => ShowSafe [a] where
  showsSafePrec xs _ = showSafeList xs

instance ShowSafe Char where
  showsSafePrec x _ = renHash (fromString [x])
  showSafeList xs = renHash (fromString xs)

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

instance ShowSafe Any

instance ShowSafe All
