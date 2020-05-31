{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
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
-- Public classes and instances
--

class ShowSafe a where
  showsSafePrec :: Int -> a -> Renderer
  default showsSafePrec ::
    (Generic a, GenShowSafe (Rep a)) =>
    Int ->
    a ->
    Renderer
  showsSafePrec p = gssPrec Pref p . from

  showsSafe :: a -> Renderer
  showsSafe = showsSafePrec 0

  showSafe :: a -> Text
  showSafe x = appRen (showsSafe x :: Renderer) mempty

  showSafeList :: [a] -> Renderer
  showSafeList xs =
    newRen "["
      <> mconcat (intersperse (newRen ",") $ (showsSafePrec 0) <$> xs)
      <> newRen "]"

-- Derived instances
--instance ShowSafe Bool

--
-- Private Generic classes and instances
--
class GenShowSafe f where
  gssPrec :: ConsKind -> Int -> f p -> Renderer
  isNullary :: f p -> Bool

instance GenShowSafe V1 where
  gssPrec _ _ _ = mempty
  isNullary = const True

instance GenShowSafe U1 where
  gssPrec _ _ _ = mempty
  isNullary = const True

instance (ShowSafe a) => GenShowSafe (K1 i a) where
  gssPrec _ p x = showsSafePrec p $ unK1 x
  isNullary = const False
---- GHC.Generic meta
--instance (GenShowSafe f) => GenShowSafe (M1 D meta f) where
--  genShowSafeS = genShowSafeS . unM1
--
--instance (GenShowSafe f, Constructor meta) => GenShowSafe (M1 C meta f) where
--  genShowSafeS x =
--    newRen (pack $ conName x) <> genShowSafeS (unM1 x)
--
--instance (GenShowSafe f) => GenShowSafe (M1 S meta f) where
--  genShowSafeS = genShowSafeS . unM1
--
--instance (GenShowSafe a, GenShowSafe b) => GenShowSafe (a :+: b) where
--  genShowSafeS (L1 x) = genShowSafeS x
--  genShowSafeS (R1 x) = genShowSafeS x
