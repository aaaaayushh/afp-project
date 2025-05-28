module DBEnv where

import qualified Data.Map as Map
import Lang.Abs (Ident)

-- For values: use list (index-based lookup)
type DBEnv a = [a]

-- For functions: still use map (function names preserved)
type FunEnv a = Map.Map Ident a

-- De Bruijn environment operations
emptyDB :: DBEnv a
emptyDB = []

lookupDB :: Int -> DBEnv a -> Maybe a
lookupDB i env
  | i < 0 || i >= length env = Nothing
  | otherwise = Just (env !! i)

extendDB :: a -> DBEnv a -> DBEnv a
extendDB x env = x : env -- prepend (index 0 is most recent)

-- Function environment operations (unchanged from regular Env)
emptyFun :: FunEnv a
emptyFun = Map.empty

lookupFun :: Ident -> FunEnv a -> Maybe a
lookupFun = Map.lookup

bindFun :: Ident -> a -> FunEnv a -> FunEnv a
bindFun = Map.insert