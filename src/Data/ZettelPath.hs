module Data.ZettelPath where

import Data.Path

import Control.Lens
       (Iso', Lens', iso, lens, set, view)

newtype ZettelRoot = ZettelRoot (Path Root Dir)

_ZettelRoot :: Iso' (Path Root Dir) ZettelRoot
_ZettelRoot = iso ZettelRoot (\(ZettelRoot r) -> r)

class HasZettelRoot a where
  {-# MINIMAL getZettelRoot, setZettelRoot | zettelRoot #-}
  getZettelRoot :: a -> ZettelRoot
  getZettelRoot = view zettelRoot

  setZettelRoot :: a -> ZettelRoot -> a
  setZettelRoot = flip (set zettelRoot)

  zettelRoot :: Lens' a ZettelRoot
  zettelRoot = lens getZettelRoot setZettelRoot

instance HasZettelRoot ZettelRoot where
  zettelRoot = lens id (\_ b -> b)
