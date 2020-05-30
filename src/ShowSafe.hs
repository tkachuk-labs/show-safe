module ShowSafe
  ( module Import,
    showSafe,
  )
where

--
-- TODO : import only needed stuff
--
import ShowSafe.Class as Import
import ShowSafe.Data as Import
import ShowSafe.Import

showSafe :: (ShowSafe a) => a -> Text
showSafe x = renderer mempty
  where
    renderer :: Text -> Text
    renderer = coerce $ showSafeS x
