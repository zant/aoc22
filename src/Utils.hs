module Utils (HMap, makeMap) where

import qualified Data.Map as Map

-- | HashMap
type HMap a = Map.Map a a 

makeMap :: Ord a => [a] -> HMap a
makeMap = Map.fromList . map (\x -> (x,x))


