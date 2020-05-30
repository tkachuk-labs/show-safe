{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module ShowSafe.Class
  ( ShowSafe (..),
  )
where

import ShowSafe.Class.Generic
import ShowSafe.Import

class ShowSafe a where
  showSafeS :: a -> Renderer
  default showSafeS :: (Generic a, GenShowSafe (Rep a)) => a -> Renderer
  showSafeS = genShowSafeS . from

-- Derived instances
instance ShowSafe Bool
