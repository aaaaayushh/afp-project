module CustomMap where

import Lang.Abs (Ident (..))

-- | A simple key-value map implemented as an association list.
type CustomMap k v = [(k, v)]

-- | An empty map.
empty :: CustomMap k v
empty = []

-- | Insert a key-value pair into the map.
-- If the key already exists, its value is updated.
insert :: (Eq k) => k -> v -> CustomMap k v -> CustomMap k v
insert key value map = (key, value) : filter ((/= key) . fst) map

-- | Look up a value by its key.
lookup :: (Eq k) => k -> CustomMap k v -> Maybe v
lookup _ [] = Nothing
lookup key ((k, v) : rest)
  | key == k = Just v
  | otherwise = CustomMap.lookup key rest