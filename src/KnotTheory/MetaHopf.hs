module KnotTheory.MetaHopf where

import KnotTheory.PD
import Data.Maybe (mapMaybe) 

-- Define the notion of a meta-Hopf algebra expression

data MetaHopf i = Mult i i i
                | CoMult i i i
                | Unit i
                | CoUnit i
                | Antipode i
                | R  i i
                | R' i i
                | Kink Int i
                | Tr i
                | Rename i i
                deriving (Show, Eq)

type MetaHopfExpression i = [MetaHopf i]

-- Honestly, this is a lot of work for something I could implement more quickly
-- in Mathematica™. I'll just do that for now.

toMetaHopfExpression :: RVT i -> MetaHopfExpression i
toMetaHopfExpression (RVT cs xs rs) = map Unit (skeletonIndices cs)
  ++ mapMaybe toKink rs
  ++ map toR xs
  ++ concatMap toMult cs
  ++ mapMaybe traces cs
  where toKink (i,0) = Nothing
        toKink (i,n) = Just (Kink n i)
        toR (Xp i j) = R  i j
        toR (Xm i j) = R' i j
        toMult c = let (i:is) = toList c in map (\j -> Mult i j i) is
        traces (Strand _) = Nothing
        traces (Loop (i:_)) = Just (Tr i)
