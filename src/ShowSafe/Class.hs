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
-- Public classes
--

appPrec :: Int
appPrec = 2

showsSafePrecDefault ::
  (Generic a, GenShowSafe (Rep a)) =>
  Int ->
  a ->
  Renderer
showsSafePrecDefault p = gssPrec Pref p . from

class ShowSafe a where
  showsSafePrec :: Int -> a -> Renderer
  default showsSafePrec ::
    (Generic a, GenShowSafe (Rep a)) =>
    Int ->
    a ->
    Renderer
  showsSafePrec = showsSafePrecDefault

  showsSafe :: a -> Renderer
  showsSafe = showsSafePrec 0

  showSafe :: a -> Text
  showSafe x = appRen (showsSafe x :: Renderer) mempty

  showSafeList :: [a] -> Renderer
  showSafeList xs =
    newRen "["
      <> mconcat (intersperse (newRen ",") $ showsSafePrec 0 <$> xs)
      <> newRen "]"

--
-- Public instances
--

instance ShowSafe Bool

instance ShowSafe ()

instance (ShowSafe a, ShowSafe b) => ShowSafe (a, b)

instance (ShowSafe a, ShowSafe b, ShowSafe c) => ShowSafe (a, b, c)

instance ShowSafe a => ShowSafe [a] where
  showsSafePrec _ = showSafeList

instance (ShowSafe (f p), ShowSafe (g p)) => ShowSafe ((f :+: g) p)

instance (ShowSafe (f p), ShowSafe (g p)) => ShowSafe ((f :*: g) p)

instance ShowSafe (f (g p)) => ShowSafe ((f :.: g) p)

--
-- Private Generic classes and instances
--
class GenShowSafe f where
  isNullary :: f p -> Bool
  gssPrec :: ConsKind -> Int -> f p -> Renderer

instance GenShowSafe V1 where
  isNullary = const True
  gssPrec _ _ _ = mempty

instance GenShowSafe U1 where
  isNullary = const True
  gssPrec _ _ _ = mempty

instance (ShowSafe a) => GenShowSafe (K1 i a) where
  isNullary = const False
  gssPrec _ p x = showsSafePrec p $ unK1 x

instance (GenShowSafe a, Constructor c) => GenShowSafe (M1 C c a) where
  isNullary = const False
  gssPrec _ n c@(M1 x) =
    case fixity of
      Prefix ->
        parenRen
          (n > appPrec && not (isNullary x))
          ( newRen (pack $ conName c)
              <> (if isNullary x then mempty else newRen " ")
              <> showBraces t (gssPrec t appPrec x)
          )
      Infix _ m -> parenRen (n > m) (showBraces t (gssPrec t m x))
    where
      fixity = conFixity c
      t =
        if (conIsRecord c)
          then Rec
          else case (conIsTuple c) of
            True -> Tup
            False -> case fixity of
              Prefix -> Pref
              Infix _ _ -> Inf (show (conName c)) -- is this show needed?
      showBraces :: ConsKind -> Renderer -> Renderer
      showBraces Rec p = newRen "{" <> p <> newRen "}"
      showBraces Tup p = newRen "(" <> p <> newRen ")"
      showBraces Pref p = p
      showBraces (Inf _) p = p
      conIsTuple :: (Constructor c) => C1 c f p -> Bool
      conIsTuple = isTupleName . conName
        where
          isTupleName ('(' : ',' : _) = True
          isTupleName _ = False

instance (Selector s, GenShowSafe a) => GenShowSafe (M1 S s a) where
  isNullary = isNullary . unM1
  gssPrec t n s@(M1 x)
    | selName s == "" = --showParen (n > appPrec)
      gssPrec t n x
    | otherwise =
      newRen (pack $ selName s)
        <> newRen " = "
        <> gssPrec t 0 x

instance (GenShowSafe a) => GenShowSafe (M1 D d a) where
  isNullary = isNullary . unM1
  gssPrec t n = gssPrec t n . unM1

instance (GenShowSafe a, GenShowSafe b) => GenShowSafe (a :+: b) where
  isNullary (L1 x) = isNullary x
  isNullary (R1 x) = isNullary x
  gssPrec t n (L1 x) = gssPrec t n x
  gssPrec t n (R1 x) = gssPrec t n x

instance (GenShowSafe a, GenShowSafe b) => GenShowSafe (a :*: b) where
  isNullary = const False
  gssPrec t@Rec n (a :*: b) =
    gssPrec t n a <> newRen ", " <> gssPrec t n b
  gssPrec t@(Inf s) n (a :*: b) =
    gssPrec t n a <> newRen s <> gssPrec t n b
  gssPrec t@Tup n (a :*: b) =
    gssPrec t n a <> newRen "," <> gssPrec t n b
  gssPrec t@Pref n (a :*: b) =
    gssPrec t (n + 1) a <> newRen " " <> gssPrec t (n + 1) b
