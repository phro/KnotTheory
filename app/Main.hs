{-# LANGUAGE FlexibleInstances #-}
module Main where
import KnotTheory.PD
import KnotTheory.NamedKnots

class MMA a where
  toMMA :: a -> String

instance (Show i) => MMA (Xing i) where
  toMMA (Xp i j) = "Xp" ++ show [i,j]
  toMMA (Xm i j) = "Xm" ++ show [i,j]

instance (Show i) => MMA (Component i) where
  toMMA = show

instance (MMA a) => MMA [a] where
  toMMA x = showMMAList x ""

instance (Show i, Show j) => MMA ((,) i j) where
  toMMA (i,j) = "{"++ show i ++ ", " ++ show j ++ "}"

showMMAList :: (MMA a) => [a] -> ShowS
showMMAList = showMMAList__ (\x y -> toMMA x ++ y)
showMMAList__ :: (a -> ShowS) ->  [a] -> ShowS
showMMAList__ _     []     s = "{}" ++ s
showMMAList__ showx (x:xs) s = '{' : showx x (showl xs)
  where
    showl []     = '}' : s
    showl (y:ys) = ", " ++ showx y (showl ys)

-- instance (Show i, KnotObject k) => MMA (k i) where
instance (Show i) => MMA (SX i) where
  toMMA (SX  cs xs) = "SXForm["++ toMMA cs ++", "++ toMMA xs ++"]"

instance (Show i) => MMA (RVT i) where
  toMMA (RVT cs xs rs) = "RVT["++ toMMA cs ++", "++ toMMA xs ++", "++ toAssoc rs++"]"

showMMAAssoc :: (Show i, Show a) => [(i,a)] -> ShowS
showMMAAssoc = showMMAAssoc__ (\(i,a) y -> show i ++ " -> " ++ show a ++ y)
showMMAAssoc__ :: (a -> ShowS) ->  [a] -> ShowS
showMMAAssoc__ _     []     s = "<||>" ++ s
showMMAAssoc__ showx (x:xs) s = "<|" ++ showx x (showl xs)
  where
    showl []     = "|>" ++ s
    showl (y:ys) = ", " ++ showx y (showl ys)

toAssoc :: (Show i, Show a) => [(i,a)] -> String
toAssoc x = showMMAAssoc x ""

openFirstStrand :: SX i -> SX i
openFirstStrand (SX (c:cs) xs) = SX cs' xs
  where cs' = Strand (toList c) : cs
openFirstStrand _              = error "knot object has no strands"

main :: IO ()
-- main = putStrLn $ "rvks = " ++ (toMMA . map toRVT) allKnots
-- main = putStrLn $ "rvks = " ++ (toMMA . map toRVT) allKnots
-- main = putStrLn $ "rvksLinks = " ++ toMMA (map (toRVT . openFirstStrand) allLinks)
-- main = putStrLn $ "rvksLinks = " ++ toMMA (map toRVT allLinks)
-- main = print . toRVT $ SX [Strand [1,2], Loop[3,4]] [Xp 1 3, Xm 2 4]

toTangledObject :: String -> NamedKnot -> String
toTangledObject s (n, a, xs)  = s++"["++
        show n++", "++(if a then "" else "Non")++"Alternating, "++show xs++"]"

toKnot :: NamedKnot -> String
toKnot = toTangledObject "Knot"

toLink :: NamedKnot -> String
toLink = toTangledObject "Link"

main = let eqs = ks ++ ls
           ks  = map (\(k,sx) ->  "toRVT["++toKnot k++"] = "++ mr sx) namedKnots
           ls  = map (\(l,sx) ->  "toRVT["++toLink l++"] = "++ mr sx) namedLinks
           mr = toMMA . toRVT . openFirstStrand
        in
           mapM_ putStrLn eqs
