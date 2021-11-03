-- module KnotTheory ( Xing (Xp, Xm, Xv)
                  -- , KnotObject (SX, RVT)
                  -- , Component (Strand, Loop)
                  -- , Strand
                  -- , Loop
                  -- , isPositive
                  -- , isNegative
                  -- , sign
                  -- , skeleton
                  -- , xings
                  -- , rotnums
                  -- , toSX
                  -- , toRVT
                  -- , isRVT
                  -- , isSX
                  -- ) where
module KnotTheory where
import Data.Maybe (listToMaybe, catMaybes)
import Data.List (find, groupBy, delete)
import Data.Function (on)

data Xing i = Xp i i | Xm i i -- | Xv i i
  deriving (Eq, Show)

sign :: (Integral b) => Xing i -> b
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
  deriving (Eq, Show)

data KnotObject i = SX { skeleton :: Skeleton i
                       , xings :: [Xing i]
                       }
                  | RVT { skeleton :: Skeleton i
                        , xings :: [Xing i]
                        , rotnums :: [(i,Int)]
                        -- , rotnums :: i -> Int
                        }
                        deriving (Show, Eq)

-- -- TODO: Implement show instance for rotnums. Alternatively, define a function:
rotnum :: (Eq i) => KnotObject i -> i -> Maybe Int
rotnum k i = fmap snd . find (\ir -> fst ir == i) $ rotnums k

-- Question: what value should rotnums return for undefined indices? Perhaps it
-- should be a Maybe-wrapped value? For now, let's choose 0.

toList :: Component i -> [i]
toList (Strand is) = is
toList (Loop is)   = is

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

otherArc :: (Eq i) => Xing i -> i -> i
otherArc (Xm i j) k
  | k == i     = j
  | otherwise = i
otherArc (Xp i j) k
  | k == i     = j
  | otherwise = i

next :: (Eq i) => i -> Strand i -> Maybe i
next _ [] = Nothing
next i (x:y:ys)
  | i == x = Just y
  | otherwise = next i (y:ys)

prev :: (Eq i) => i -> Strand i -> Maybe i
prev _ [] = Nothing
prev i (x:y:ys)
  | i == y = Just x
  | otherwise = next i (y:ys)
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

isTerminalIndex :: (Eq i) => i -> Skeleton i -> Bool
isTerminalIndex i cs = or $
  map (\c -> i `isHeadOfComponent` c || i `isLastOfComponent` c) cs

nextSkeletonIndex :: (Eq i) => i -> Skeleton i -> Maybe i
nextSkeletonIndex i = listToMaybe . catMaybes . map (nextComponentIndex i)

prevSkeletonIndex :: (Eq i) => i -> Skeleton i -> Maybe i
prevSkeletonIndex i = listToMaybe . catMaybes . map (prevComponentIndex i)

mergeBy :: (Eq i) => ([a] -> b) -> [(i,a)] -> [(i,b)]
mergeBy f = map (wrapIndex f) . (groupBy ((==) `on` fst))
  where
    wrapIndex :: ([a] -> b) -> [(i,a)] -> (i,b)
    wrapIndex f xs@(x:ys) = (fst x, f . map snd $ xs)

type Front i = [(i,Dir)]

findNextXing :: (Eq i) => KnotObject i -> (i,Dir) -> Maybe (Xing i)
findNextXing k (i,Out) = find (`involves` i) $ xings k
findNextXing k (i,In ) = do
  i' <- prevSkeletonIndex i $ skeleton k
  find (`involves` i') $ xings k
  -- (prevSkeletonIndex i $ skeleton k) >>= (\i' -> find (`involves` i') $ xings k)

getNewFront :: (Eq i) => KnotObject i -> (i,Dir) -> Maybe (Xing i) -> (Front i,(i,Int))
getNewFront _ (_,Out) Nothing = ([],[])
getNewFront _ (i,In ) Nothing = ([],[(i,1)])
getNewFront k (i,Out) Just x
  | isPositive x == (i == underStrand x) = ([(j,In),(i',Out),(j',In)],(j,1))
getNewFront k (i,In ) Just x = _

modifyFront :: (Eq i) => KnotObject i -> (i,Dir) -> (Front i,(i,Int))
modifyFront k f = getNewFront f $ findNextXing k f

data Dir = In | Out
  deriving (Eq, Show)

{-
 - Assumptions:
 -   1. k is a link (with no open components)
 -   2. k is a planar diagram.
 -   3. k is a connected diagram.
 - toRVT outputs:
 -   1. a pure RVT.
 -   2. a planar diagram.
 -   2. if any subset of components are closed, the diagram remains planar.
 -}
toRVT :: (Eq i) => KnotObject i -> KnotObject i
toRVT r@(RVT _ _ _) = r
toRVT k@(SX cs xs) = RVT cs xs rs where
  rs = mergeBy sum $ getRotNums [(i1,In),(i1,Out)] $
    map (\i -> (i,0)) . strandIndices $ k
  i1 = head . toList . head $ cs
  getRotNums :: (Eq i) => Front i ->  [(i,Int)] -> [(i,Int)]
  getRotNums             []         rs        = rs
  getRotNums             (id:is) rs =
    case find (((==) `on` fst) id) is of
      Just id' ->
        getRotNums
          (delete id' is)
          (case snd id of
              Out -> rs
              In -> (fst id,1):rs
          )
      Nothing -> let (f,rs') = modifyFront k id in
        getRotNums (f++ is) rs'
    -- case find (((==) `on` fst) id) is of
      -- Nothing ->
        -- getRotNums
          -- (deleteBy ((==) `on` fst) id is)
          -- (case snd id of
              -- Out -> rs
              -- In -> mergeBy sum $ (fst id,1):rs
          -- )
      -- Just id' -> let (f,rs') = _ k id' in
        -- getRotNums (f++ is) rs'


strandIndices :: KnotObject i -> [i]
strandIndices = concat . map toList . skeleton

isSX :: KnotObject i -> Bool
isSX (SX _ _) = True
isSX _      = False

isRVT :: KnotObject i -> Bool
isRVT (RVT _ _ _) = True
isRVT _         = False

toSX :: KnotObject i -> KnotObject i
toSX k@(SX _ _) = k
toSX (RVT cs xs _) = SX cs xs

