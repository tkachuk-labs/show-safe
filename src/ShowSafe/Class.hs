{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module ShowSafe.Class
  ( ShowSafe (..),
  )
where

import ShowSafe.Import

class ShowSafe a where
  showSafeS :: a -> Renderer
  default showSafeS :: (Generic a, ShowSafe1 (Rep a)) => a -> Renderer
  showSafeS = showSafeS1 . from

-- The typical solution is to have a helper class
-- that works with things of * -> * kind
-- (this also removes the p parameters from signatures)
class ShowSafe1 f where
  showSafeS1 :: f p -> Renderer

instance ShowSafe1 U1 where
  showSafeS1 _ = Renderer (mempty <>)

instance (Constructor meta) => ShowSafe1 (M1 C meta f) where
  showSafeS1 x = Renderer $ \acc -> (pack $ conName x) <> acc

instance (ShowSafe1 f) => ShowSafe1 (M1 D meta f) where
  showSafeS1 = showSafeS1 . unM1

instance (ShowSafe1 f) => ShowSafe1 (M1 S meta f) where
  showSafeS1 = showSafeS1 . unM1

instance (ShowSafe1 a, ShowSafe1 b) => ShowSafe1 (a :+: b) where
  showSafeS1 (L1 x) = showSafeS1 x
  showSafeS1 (R1 x) = showSafeS1 x

instance ShowSafe Bool
