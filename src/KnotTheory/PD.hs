{-# LANGUAGE DeriveFunctor #-}
module KnotTheory.PD where
import Data.Maybe (listToMaybe, catMaybes, mapMaybe, fromMaybe, fromJust)
import Data.List (find, groupBy, sortOn, delete, deleteBy)
import Data.Tuple (swap)
import Data.Function (on)

type Index = Int
data Xing i = Xp i i | Xm i i-- | Xv i i
  deriving (Eq, Show, Functor)

-- instance Functor Xing where
  -- fmap f x =
    -- case x of
      -- Xp i j -> Xp (f i) (f j)
      -- Xm i j -> Xm (f i) (f j)

sign :: (Integral b) => Xing Index -> b
sign (Xp _ _) = 1
sign (Xm _ _) = -1
-- sign (Xv _ _) = 0

isPositive :: Xing i -> Bool
isPositive (Xp _ _) = True
isPositive (Xm _ _) = False
-- isPositive (Xv _ _) = False

isNegative :: Xing i -> Bool
isNegative (Xp _ _) = False
isNegative (Xm _ _) = True
-- isNegative (Xv _ _) = False

overStrand :: Xing i -> i
overStrand (Xp i _) = i
overStrand (Xm i _) = i
-- overStrand (Xv _ _) =

underStrand :: Xing i -> i
underStrand (Xp _ i) = i
underStrand (Xm _ i) = i

type Strand i = [i]
type Loop i = [i]
type Skeleton i = [Component i]

data Component i = Strand (Strand i) | Loop (Loop i)
  deriving (Eq, Show, Functor)

-- Q. Is it important to require these conversion programs for *any* knot object?
class KnotObject k where
  toSX  :: (Ord i, Enum i) => k i -> SX i
  toRVT :: (Ord i, Enum i) => k i -> RVT i
  toRVT = toRVT . toSX

class PD k where
  skeleton :: k i -> Skeleton i
  xings :: k i -> [Xing i]

data SX  i = SX  (Skeleton i) [Xing i] deriving (Show, Eq, Functor)
data RVT i = RVT (Skeleton i) [Xing i] [(i,Int)] deriving (Show, Eq, Functor)

reindex :: (PD k, Functor k, Eq i) => k i -> k Int
reindex k = fmap (fromJust . flip lookup table) k
  where
    table = zip (strandIndices s) [1..]
    s = skeleton k

-- data SX  = SX (Skeleton Index) [Xing Index] deriving (Show, Eq)
-- data RVT = RVT (Skeleton Index) [Xing Index] [(Index,Int)] deriving (Show, Eq)

instance KnotObject SX where
  toSX = id
  toRVT k@(SX cs xs) = RVT cs xs rs where
    rs = mergeBy sum $ getRotNums k [(i1,Out)]
        -- ++ [ (i, 0) | i <- strandIndices cs ]
        ++ [ (i, δ i i1) | i <- strandIndices cs ]
    i1 = head . toList $ s
    Just s = find isStrand cs

instance KnotObject RVT where
  toRVT = id
  toSX (RVT s xs _) = SX s xs

instance PD SX where
  skeleton (SX s _) = s
  xings (SX _ xs)   = xs

instance PD RVT where
  skeleton (RVT s _ _) = s
  xings (RVT _ xs _)   = xs

rotnums :: RVT i -> [(i,Int)]
rotnums (RVT _ _ rs) = rs

rotnum :: (Eq i) => RVT i -> i -> Int
-- rotnum k i = fmap snd . find (\ir -> fst ir == i) $ rotnums k
rotnum k i = fromMaybe 0 . lookup i . rotnums $ k

isStrand :: Component i -> Bool
isStrand (Strand _) = True
isStrand _          = False

isLoop :: Component i -> Bool
isLoop (Loop _) = True
isLoop _        = False

toList :: Component i -> [i]
toList (Strand is) = is
toList (Loop is)   = is

strandIndices :: Skeleton i -> [i]
strandIndices = concatMap toList

-- involves :: (Eq i) => Skeleton i -> Xing i -> i -> Bool
-- involves s x i = or $ map (Just i==)
  -- [Just o, Just u, nextSkeletonIndex o s, prevSkeletonIndex u s]
    -- where o = overStrand x
          -- u = underStrand x
involves :: (Eq i) => Xing i -> i -> Bool
Xp i j `involves` k = i == k || j == k
Xm i j `involves` k = i == k || j == k
-- Xv i j `involves` k = i == k || j == k
-- Can the duplication be avoided here?

-- safeOtherArc :: Xing i -> i -> Maybe i
-- safeOtherArc (Xm i j) k
  -- | k == i     = Just j
  -- | k == j     = Just i
  -- | otherwise = Nothing
-- safeOtherArc (Xp i j) k
  -- | k == i     = Just j
  -- | k == j     = Just i
  -- | otherwise = Nothing

otherArc :: (Eq i) => Xing i -> i -> Maybe i
otherArc x i
  | i == o     = Just u
  | i == u     = Just o
  | otherwise = Nothing
  where o = overStrand x
        u = underStrand x

next :: (Eq i) => i -> Strand i -> Maybe i
next _ []  = Nothing
next _ [_] = Nothing
next i (x:y:ys)
  | i == x = Just y
  | otherwise = next i (y:ys)

prev :: (Eq i) => i -> Strand i -> Maybe i
prev _ []  = Nothing
prev _ [_] = Nothing
prev i (x:y:ys)
  | i == y = Just x
  | otherwise = prev i (y:ys)
-- next e = listToMaybe . drop 1 . dropWhile (/= e)

nextCyc :: (Eq i) => i -> Loop i -> Maybe i
nextCyc e xs = next e . take (length xs + 1). cycle $ xs

prevCyc :: (Eq i) => i -> Loop i -> Maybe i
prevCyc e xs = prev e . take (length xs + 1). cycle $ xs

-- Philosophical question: should I be naming all these boolean functions or
-- should those be ebmedded into functions where this is wanted?

isHeadOf :: (Eq i) => i -> [i] -> Bool
x `isHeadOf` ys = x == head ys

isLastOf:: (Eq i) => i -> [i] -> Bool
x `isLastOf` ys = x == last ys

nextComponentIndex :: (Eq i) => i -> Component i -> Maybe i
nextComponentIndex i (Strand is) = next i is
nextComponentIndex i (Loop is) = nextCyc i is

prevComponentIndex :: (Eq i) => i -> Component i -> Maybe i
prevComponentIndex i (Strand is) = prev i is
prevComponentIndex i (Loop is) = prevCyc i is

isHeadOfComponent :: (Eq i) => i -> Component i -> Bool
isHeadOfComponent _ (Loop   _ ) = False
isHeadOfComponent i (Strand is) = i `isHeadOf` is

isLastOfComponent :: (Eq i) => i -> Component i -> Bool
isLastOfComponent _ (Loop   _ ) = False
isLastOfComponent i (Strand is) = i `isLastOf` is

isTerminalOfComponent :: (Eq i) => Component i -> i ->  Bool
isTerminalOfComponent c i = i `isHeadOfComponent` c || i `isLastOfComponent` c

isTerminalIndex :: (Eq i) => Skeleton i -> i ->  Bool
isTerminalIndex cs i = any (`isTerminalOfComponent` i) cs

nextSkeletonIndex :: (Eq i) => Skeleton i -> i -> Maybe i
nextSkeletonIndex s i = listToMaybe . mapMaybe (nextComponentIndex i) $ s

prevSkeletonIndex :: (Eq i) => Skeleton i -> i -> Maybe i
prevSkeletonIndex s i = listToMaybe . mapMaybe (prevComponentIndex i) $ s

{-
 - Assumptions:
 -   1. k is a (1,n)-tangle (a tangle with only one open component)
 -   2. k is a planar diagram.
 -   3. k is a connected diagram.
 - toRVT outputs:
 -   1. a planar (1,n)-rotational virtual tangle
 -}

δ :: (Eq a) => a -> a -> Int
δ x y
  | x == y     = 1
  | otherwise = 0

-- toRVT k = toRVT . toSX $ k

mergeBy :: (Ord i) => ([a] -> b) -> [(i,a)] -> [(i,b)]
mergeBy f = map (wrapIndex f) . groupBy ((==) `on` fst) . sortOn fst
  where
    wrapIndex :: ([a] -> b) -> [(i,a)] -> (i,b)
    wrapIndex f xs@(x:ys) = (fst x, f . map snd $ xs)

type Front i = [(i,Dir)]

getRotNums :: (Eq i) => SX i -> Front i -> [(i,Int)]
getRotNums k = fst . until (null . snd) (>>= advanceFront k) . return

advanceFront :: (Eq i) => SX i -> Front i -> ([(i,Int)], Front i)
advanceFront _ []           = return []
advanceFront k f@((i,d):fs) =
  case find ((== i) . fst) fs of
    Just (i,In ) -> (return (i,-1), fs')
    Just (i,Out) -> return fs'
    Nothing      ->
      case findNextXing k (i,d) of
        Just x  -> absorbXing k x f
        Nothing -> return fs
  where fs' = deleteBy ((==) `on` fst) (i,d) fs
  -- where fs' = delete id' fs

absorbXing :: (Eq i) => SX i -> Xing i -> Front i -> ([(i,Int)],Front i)
absorbXing k x ((i,d):fs) =
  case d of
    Out
      | isPositive x == (i == underStrand x) ->
        (f [(j,1)],f [(j ,In ),(i',Out),(j',Out)] ++ fs)
      | otherwise ->
        return $ f [(j',Out),(i',Out),(j ,In )] ++ fs
      where
        i' = nextSkeletonIndex s i
        j  = otherArc x i
        j' = j >>= nextSkeletonIndex s
    In
      | isPositive x == (i' == Just (overStrand x)) ->
        (f [(j,1)],f [(j ,In ),(i',In),(j',Out)] ++ fs)
      | otherwise ->
        return $ f [(j',Out),(i',In),(j ,In )] ++ fs
      where
        i' = prevSkeletonIndex s i
        j  = i' >>= otherArc x
        j' = j  >>= nextSkeletonIndex s
  where
    f  = mapMaybe (fmap swap . sequence . swap)
    s  = skeleton k

data Dir = In | Out
  deriving (Eq, Show)
-- There should be cleaner functions to deal with xings and their associated
-- strands.

findNextXing :: (Eq i, PD k, KnotObject k) => k i -> (i,Dir) -> Maybe (Xing i)
findNextXing k (i,Out) = find (`involves` i) $ xings k
findNextXing k (i,In ) = do
  i' <- prevSkeletonIndex (skeleton k) i
  find (`involves` i') $ xings k
  -- (prevSkeletonIndex i $ skeleton k) >>= (\i' -> find (`involves` i') $ xings k)
