module Env where

import CustomMap qualified as Map
import Lang.Abs (Ident)

type Env a = Map.CustomMap Ident a

empty :: Env a
empty = Map.empty

find :: Ident -> Env a -> Maybe a
find = Map.lookup

bind :: Ident -> a -> Env a -> Env a
bind = Map.insert
