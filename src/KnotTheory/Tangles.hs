module KnotTheory.Tangles where
import KnotTheory.PD
import Data.List

applyAt :: (a -> a) -> Int  -> [a] -> [a]
applyAt _ _ [] = []
applyAt f 0 (x:xs) = f x : xs
applyAt f n (x:xs) = x : applyAt f (n-1) xs

diag :: (a -> a) -> [a] -> [[a]]
diag f xs = map (\i -> applyAt f i xs) [0..n-1]
        where n = length xs

cycles :: [a] -> [[a]]
cycles x = x:map (\n -> take l . drop n $ xs) [1.. l-1]
        where xs = cycle x
              l  = length x

compCuts :: (Eq i) => Skeleton i -> Component i -> [Skeleton i]
compCuts s c = map ((: delete c s). Strand) . cycles . toList $ c

allCuts :: (Eq i) => Skeleton i -> [[Skeleton i]]
allCuts s = map (compCuts s) s

getSplits :: (Eq i) => SX i -> [[SX i]]
getSplits (SX s xs) = map (map (\s -> SX s xs)) $ allCuts s

toRVTSplits :: SX Int -> [[RVT Int]]
toRVTSplits = map (map (reindex . toRVT)) . getSplits

toRVTs :: SX Int -> [RVT Int]
toRVTs = map (toRVT . head) . getSplits
