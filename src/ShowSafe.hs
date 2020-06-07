module ShowSafe (module Import, showSafeDef) where

--
-- TODO : import only needed stuff
--
import ShowSafe.Class as Import
import ShowSafe.Data as Import
import ShowSafe.Import

showSafeDef :: (ShowSafe a, IsString s, Monoid s) => a -> s
showSafeDef x = showSafe x SHA256 $ Just $ Salt "tkachuk.labs"
