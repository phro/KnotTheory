We begin with a series of imports of common functions, relating to list
manipulations and type-wrangling. The exact details are not too important.
\begin{code}
{-# LANGUAGE DeriveFunctor #-}
module KnotTheory.PD where
import Data.Maybe (listToMaybe, catMaybes, mapMaybe, fromMaybe, fromJust)
import Data.List (find, groupBy, sortOn, partition, intersect, union, (\\))
import Data.Tuple (swap)
import Data.Function (on)
import Control.Monad ((>=>))
import Control.Arrow ((>>>))
\end{code}
Next, we introduce the crossing type, which can be either positive \hs{Xp} or
negative \hs{Xm} (using the mnemonic \enquote{plus} and \enquote{minus}):
\begin{code}
type Index = Int
data Xing i = Xp i i | Xm i i -- | Xv i i
  deriving (Eq, Show, Functor)
\end{code}
We define several functions which extract basic data from a crossing.
\begin{code}
sign :: (Integral b) => Xing Index -> b
sign (Xp _ _) = 1
sign (Xm _ _) = -1

isPositive :: Xing i -> Bool
isPositive (Xp _ _) = True
isPositive (Xm _ _) = False

isNegative :: Xing i -> Bool
isNegative (Xp _ _) = False
isNegative (Xm _ _) = True

overStrand :: Xing i -> i
overStrand (Xp i _) = i
overStrand (Xm i _) = i

underStrand :: Xing i -> i
underStrand (Xp _ i) = i
underStrand (Xm _ i) = i
\end{code}
Next, we introduce the notion of a planar diagram, whose data is comprised of a
collection of \hs{Strand}s and \hs{Loop}s (indexed by some type \hs{i},
typically an integer). The \hs{Skeleton} of a planar diagram is defined to be
the collection of \hs{Component}s, each of which is either an open \hs{Strand}
or a closed \hs{Loop}.
\begin{code}
type Strand i = [i]
type Loop i = [i]
data Component i = Strand (Strand i) | Loop (Loop i)
  deriving (Eq, Show, Functor)

type Skeleton i = [Component i]
\end{code}
Next, we introduce the notion of a \hs{KnotObject}, which has its components
labelled by the same type \hs{i}. We further define a function \hs{toRVT} which
converts a generic \hs{KnotObject} into an upright tangle (in this codebase,
the term \ac{RVT} is frequently used for the notion of an upright tangle). We
call an object a \defi{planar diagram} (or \hs{PD}) if it has a notion of
\hs{Skeleton} and a collection of crossings.
\begin{code}
class KnotObject k where
  toSX  :: (Ord i) => k i -> SX i
  toRVT :: (Ord i) => k i -> RVT i
  toRVT = toRVT . toSX

class PD k where
  skeleton :: k i -> Skeleton i
  xings :: k i -> [Xing i]

\end{code}
The \hs{SX} form of a diagram just contains the \hs{Skeleton} and the \hs{Xing}s
(crossings), while the \hs{RVT} form also assigns each arc an integral rotation
number.
\begin{code}
data SX  i = SX  (Skeleton i) [Xing i] deriving (Show, Eq, Functor)
data RVT i = RVT (Skeleton i) [Xing i] [(i,Int)] deriving (Show, Eq, Functor)
\end{code}
Given any labelling of the arcs in a diagram, we can re-label the arcs using
consecutive whole numbers. This is accomplised with \hs{reindex}:
\begin{code}
reindex :: (PD k, Functor k, Eq i) => k i -> k Int
reindex k = fmap (fromJust . flip lookup table) k
  where
    table = zip (skeletonIndices s) [1..]
    s = skeleton k
\end{code}
Most importantly, we now declare that a diagram expressed in \hs{SX} form (that
is, without any rotation data) may be assigned rotation numbers to each of its
arcs in a meaningful way. The bulk of the work is done by \hs{getRotNums}, which
is defined farther below. We handle the case where the entire tangle is a single
crossingless strand separately.
\begin{code}
instance KnotObject SX where
  toSX = id
  toRVT k@(SX cs xs) = RVT cs xs rs where
    rs = filter ((/=0) . snd) . mergeBy sum $ getRotNums k f1
    i1 = head . toList $ s
    Just s = find isStrand cs
    f1 = case next i1 (toList s) of
            Just _  -> [(Out,i1)]
            Nothing -> []

instance KnotObject RVT where
  toRVT = id
  toSX (RVT s xs _) = SX s xs

instance PD SX where
  skeleton (SX s _) = s
  xings (SX _ xs)   = xs

instance PD RVT where
  skeleton (RVT s _ _) = s
  xings (RVT _ xs _)   = xs
\end{code}
Next, we include a series of functions which answer basic questions about
planar diagrams. Note in \hs{rotnum}, if a rotation number is not present in the
table of values, it is assumed to be $0$.
\begin{code}
rotnums :: RVT i -> [(i,Int)]
rotnums (RVT _ _ rs) = rs

rotnum :: (Eq i) => RVT i -> i -> Int
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

skeletonIndices :: Skeleton i -> [i]
skeletonIndices = concatMap toList

involves :: (Eq i) => Xing i -> i -> Bool
x `involves` k = k `elem` [underStrand x, overStrand x]

otherArc :: (Eq i) => Xing i -> i -> Maybe i
otherArc x i
  | i == o     = Just u
  | i == u     = Just o
  | otherwise = Nothing
  where o = overStrand x
        u = underStrand x

next :: (Eq i) => i -> Strand i -> Maybe i
next e = listToMaybe . drop 1 . dropWhile (/= e)

prev :: (Eq i) => i -> Strand i -> Maybe i
prev e = next e . reverse

nextCyc :: (Eq i) => i -> Loop i -> Maybe i
nextCyc e xs = next e . take (length xs + 1). cycle $ xs

prevCyc :: (Eq i) => i -> Loop i -> Maybe i
prevCyc e xs = prev e . take (length xs + 1). cycle $ xs

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
\end{code}
In order to obtain all the crossing indices, we must take every combination of
the under- and over-strands and their following indices:
\begin{code}
getXingIndices :: (Eq i) => Skeleton i -> Xing i -> [i]
getXingIndices s x = catMaybes
        [ f a | f <- [id, (>>= nextSkeletonIndex s)], a <- [o, u] ]
        where o = return (overStrand x)
              u = return (underStrand x)

δ :: (Eq a) => a -> a -> Int
δ x y
  | x == y    = 1
  | otherwise = 0

mergeBy :: (Ord i) => ([a] -> b) -> [(i,a)] -> [(i,b)]
mergeBy f = map (wrapIndex f) . groupBy ((==) `on` fst) . sortOn fst
  where
    wrapIndex :: ([a] -> b) -> [(i,a)] -> (i,b)
    wrapIndex g xs@(x:_) = (fst x, g . map snd $ xs)
\end{code}
Here we come to the main function, \hs{getRotNums}, for which we have the
following requirements (not expressed in the code):
\begin{enumerate}
        \item The diagram \hs{k} is a $(1,n)$-tangle (a tangle with only one open component)
        \item The underlying graph of \hs{k} is a planar. 
        \item The diagram \hs{k} is a connected.
\end{enumerate}
Only in this case will the function \hs{toRVT} will then output a planar
$(1,n)$-upright tangle which corresponds to a classical (i.e. planar)
diagram.

This function involves taking a simple open curve (a Jordan curve passing
through infinity) called the \hs{Front}, and passing it over arcs in the
diagram. This curve is characterized by the arcs it passes through, together
with their orientations. Each intersection of the \hs{Front} with the diagram
provides a different \hs{View}, either \hs{In} or \hs{Out} of the \hs{Front}
when following the orientation of the intersecting arc.
\begin{figure}[h]
\centering
\includegraphics{figures/upright_41_2b.pdf}
\caption{A tangle with a front passing over it. The portion of the tangle below
the front has all crossings in upright form. All arcs fully below the front have
an integer rotation number.}
\label{fig:upright_tangle_example}
\end{figure}
\begin{code}
type Front i = [View i]
type View  i = (Dir, i)
\end{code}
We obtain the rotation numbers by successively passing the front across new
crossings (achieved by \hs{advanceFront}), keeping track of the rotation numbers
of arcs which have already passed by the front. Once the front has passed across
every crossing, all the rotation numbers have been computed.

Next, we define \hs{converge}, which iterates a function until a fixed point is
achieved.
\begin{code}
converge :: (Eq a) => (a -> a) -> a -> a
converge f x
        | x == x'   = x
        | otherwise = converge f x'
        where x' = f x
\end{code}
The function \hs{convergeT} wraps \hs{converge} in monadic transformations. In
our context, the monad will be used to keep track of rotation numbers of the
arcs.
\begin{code}
convergeT :: (Monad m, Eq (m a)) => (a -> m a) -> a -> m a
convergeT f = return >>> converge (>>= f)
\end{code}

The implementation of \hs{getRotNums} takes a front and advances it along a
diagram until no more changes occur.
\begin{code}
getRotNums :: (Eq i) => SX i -> Front i -> [(i,Int)]
getRotNums k = convergeT (advanceFront k) >>> fst
\end{code}
When advancing the \hs{Front}, we start by absorbing arcs that intersect with the
front twice until the leftmost \hs{View} no longer connects directly back to the
\hs{Front}. At this point, we can absorb a crossing into the front.
\begin{code}
advanceFront :: (Eq i) => SX i -> Front i -> ([(i,Int)], Front i)
advanceFront k = convergeT (absorbArc k) >=> absorbXing k
\end{code}
We next check for the case where the leftmost arc connects back to the
\hs{Front}. If it is pointing \hs{Out} (and therefore connects back \hs{In}
further to the right), we adjust the rotation number of the arc by $-1$.
Otherwise, we leave both the \hs{Front} and the rotation numbers unchanged.
\begin{figure}[h]
\centering
\includegraphics{figures/absorb_double_arc.pdf}
\caption{Example of absorbing an arc which intersects the front multiple times.
If the horizontal tangent vector points to to right, as in this picture, then
the rotation number of the arc is decreased by $1$. Otherwise, no change in the
rotation number is recorded.}
\label{fig:absorb_double_arc}
\end{figure}
\begin{code}
absorbArc :: (Eq i) => SX i -> Front i -> ([(i,Int)],Front i)
absorbArc k []     = return []
absorbArc k f@(f1:fs) = case fs1 of
        ( In,i):_ -> (return (i,-1), fss)
        (Out,i):_ -> return fss           -- No new rotation numbers
        []        -> return f 
        where (fs1,fss) = partition (((==) `on` snd) f1) fs
\end{code}
Our goal is to repeat this operation until we get a fixed point, which is
encoded in \hs{absorbArcs}:
\begin{code}
absorbArcs :: (Eq i) => SX i -> Front i -> ([(i,Int)],Front i)
absorbArcs k = convergeT (absorbArc k)
\end{code}

Absorb a crossing involves expanding one's view at an arc from looking at a
crossing to all the views one gets when looking in every direction at the
crossing (namely, to the left, along the arc, and to the right). The function
\hs{absorbXing} performs this task on the leftmost \hs{View} on the \hs{Front}.
The transverse strand receives a positive rotation number if it moves from left
to right. The arc receiving the rotation depends on how the crossing is
oriented.
\begin{code}
absorbXing :: (Eq i) => SX i -> Front i -> ([(i,Int)],Front i)
absorbXing _ [] = return []
absorbXing k (f:fs) = (rs,newFront++fs) where
        newFront = catMaybes [l, a, r]
        l = lookLeft k f
        a = lookAlong k f
        r = lookRight k f
        rs = case (l,f,r) of
                (Just (In,i), (Out,_),_            ) -> [(i,1)]
                (_          , (In ,_),Just (Out, j)) -> [(j,1)]
                _                                    -> [     ]

data Dir = In | Out
  deriving (Eq, Show)
\end{code}

The following functions take a \hs{View}, returning the \hs{View} one has when
looking in the corresponding direction. Since it is possible for the resulting
gaze to be merely the boundary, it is possible for these functions to return
\hs{Nothing}.
\begin{code}
lookAlong :: (Eq i, PD k) => k i -> View i -> Maybe (View i)
lookAlong k (d, i) = case d of
        Out -> sequence (Out, nextSkeletonIndex s i)
        In  -> sequence (In , prevSkeletonIndex s i)
        where s = skeleton k

lookSide :: (Eq i, PD k) => Bool -> k i -> View i -> Maybe (View i)
lookSide isLeft k di@(Out,i) = do
        x <- findNextXing k di
        j <- otherArc x i
        if isLeft == ((underStrand x == i) == isPositive x)
        then return (In, j)
        else sequence (Out, nextSkeletonIndex (skeleton k) j)
lookSide isLeft k (In,i) =
        sequence (Out, prevSkeletonIndex (skeleton k) i) >>=
        lookSide (not isLeft) k 

lookLeft  :: (Eq i, PD k) => k i -> View i -> Maybe (View i)
lookLeft = lookSide True

lookRight :: (Eq i, PD k) => k i -> View i -> Maybe (View i)
lookRight = lookSide False

findNextXing :: (Eq i, PD k) => k i -> View i -> Maybe (Xing i)
findNextXing k (Out,i) = find (`involves` i) $ xings k
findNextXing k (In ,i) = do
  i' <- prevSkeletonIndex (skeleton k) i
  find (`involves` i') $ xings k
\end{code}
