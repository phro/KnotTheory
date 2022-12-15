module KnotTheory.Tangles where
import KnotTheory.PD

applyAt :: (a -> a) -> Int  -> [a] -> [a]
applyAt _ _ [] = []
applyAt f 0 (x:xs) = f x : xs
applyAt f n (x:xs) = x : applyAt f (n-1) xs

diag :: (a -> a) -> [a] -> [[a]]
diag f xs = map (\i -> applyAt f i xs) [0..n-1]
        where n = length xs

cycles :: [a] -> [[a]]
cycles x = map (\n -> take l . drop n $ xs) [0.. l-1]
        where xs = cycle x
              l  = length x

compCuts :: (Eq i) => Component i -> Skeleton i -> [Skeleton i]
compCuts c s = map ((: delete c s). Strand) . cycles . toList $ c

toRVTSplits :: SX Int -> [RVT Int]
toRVTSplits = map (reindex . toRVT) . getSplits 

