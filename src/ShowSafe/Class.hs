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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Complex (Complex)
import Data.Monoid (Alt, Ap)
import Data.Semigroup (Arg, Max, Min)
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TS
import qualified Data.Text.Lazy as TL
import Data.Typeable (TyCon)
import ShowSafe.Import
import Type.Reflection (SomeTypeRep)

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
-- Instances similar to
-- https://hackage.haskell.org/package/base-4.14.0.0/docs/Prelude.html#t:Show
--

instance ShowSafe Bool

instance ShowSafe Char where
  showsSafePrec x _ = renHash (fromString [x])
  showSafeList xs = renHash (fromString xs)

instance ShowSafe Double where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Float where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Int where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Int8 where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Int16 where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Int32 where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Int64 where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Integer where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Natural where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Ordering

instance ShowSafe Word where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Word8 where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Word16 where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Word32 where
  showsSafePrec x _ = renShowHash x

instance ShowSafe Word64 where
  showsSafePrec x _ = renShowHash x

instance ShowSafe RuntimeRep where
  showsSafePrec x _ _ _ = renShow x

instance ShowSafe VecCount where
  showsSafePrec x _ _ _ = renShow x

instance ShowSafe VecElem where
  showsSafePrec x _ _ _ = renShow x

instance ShowSafe CallStack where
  showsSafePrec x _ _ _ = renShow x

instance ShowSafe SomeTypeRep where
  showsSafePrec x _ _ _ = renShow x

instance ShowSafe ()

instance ShowSafe TyCon where
  showsSafePrec x _ _ _ = renShow x

--
-- TODO : define other instances
-- use Generic if possible
-- if not - use renShowHash or renShow combinators
--

instance ShowSafe Associativity

instance ShowSafe Fixity

instance ShowSafe Any

instance ShowSafe All

instance ShowSafe Void

instance SS a => ShowSafe [a] where
  {-# SPECIALIZE instance ShowSafe [Char] #-}
  showsSafePrec xs _ = showSafeList xs

--
-- TODO : check parens bug (nested Maybe example)
--

instance (SS a) => ShowSafe (Maybe a)

instance (Show a) => ShowSafe (Ratio a) where
  showsSafePrec x _ = renShowHash x

instance (SS a) => ShowSafe (Par1 a)

instance (SS a) => ShowSafe (NonEmpty a)

instance (SS a) => ShowSafe (Down a)

instance (SS a) => ShowSafe (Product a)

instance (SS a) => ShowSafe (Sum a)

instance (SS a) => ShowSafe (Dual a)

instance (SS a) => ShowSafe (Last a)

instance (SS a) => ShowSafe (First a)

instance (SS a) => ShowSafe (Identity a)

instance (SS a) => ShowSafe (ZipList a)

instance (SS a) => ShowSafe (Option a)

instance (SS a) => ShowSafe (WrappedMonoid a)

instance (SS a) => ShowSafe (Max a)

instance (SS a) => ShowSafe (Min a)

instance (SS a) => ShowSafe (Complex a)

instance (SS a, SS b) => ShowSafe (Either a b)

instance ShowSafe (V1 p)

instance ShowSafe (U1 p)

instance ShowSafe (Proxy s)

instance (SS a, SS b) => ShowSafe (Arg a b)

instance SS (f p) => ShowSafe (Rec1 f p)

instance SS (f a) => ShowSafe (Alt f a)

instance SS (f a) => ShowSafe (Ap f a)

instance SS a => ShowSafe (Const a b)

instance SS c => ShowSafe (K1 i c p)

instance (SS (f p), SS (g p)) => ShowSafe ((f :+: g) p)

instance (SS (f p), SS (g p)) => ShowSafe ((f :*: g) p)

instance SS (f p) => ShowSafe (M1 i c f p)

instance SS (f (g p)) => ShowSafe ((f :.: g) p)

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

--
-- Other common instances
--

instance ShowSafe TS.Text where
  showsSafePrec x _ = renHash $ TS.encodeUtf8 x

instance ShowSafe TL.Text where
  showsSafePrec x _ = renHash $ TS.encodeUtf8 $ TL.toStrict x

instance ShowSafe BS.ByteString where
  showsSafePrec x _ = renHash x

instance ShowSafe BL.ByteString where
  showsSafePrec x _ = renHash $ BL.toStrict x
