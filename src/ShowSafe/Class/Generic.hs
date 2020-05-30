{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module ShowSafe.Class.Generic
  ( GenShowSafe (..),
  )
where

import ShowSafe.Import

-- The typical solution is to have a helper class
-- that works with things of * -> * kind
-- (this also removes the p parameters from signatures)
class GenShowSafe f where
  genShowSafeS :: f p -> Renderer

-- GHC.Generic meta
instance (GenShowSafe f) => GenShowSafe (M1 D meta f) where
  genShowSafeS = genShowSafeS . unM1

instance (GenShowSafe f, Constructor meta) => GenShowSafe (M1 C meta f) where
  genShowSafeS x =
    newRenderer (\acc -> pack (conName x) <> acc) <> genShowSafeS (unM1 x)

instance (GenShowSafe f) => GenShowSafe (M1 S meta f) where
  genShowSafeS = genShowSafeS . unM1

-- GHC.Generic data
instance GenShowSafe U1 where
  genShowSafeS _ = newRenderer (mempty <>)

instance (GenShowSafe a, GenShowSafe b) => GenShowSafe (a :+: b) where
  genShowSafeS (L1 x) = genShowSafeS x
  genShowSafeS (R1 x) = genShowSafeS x
