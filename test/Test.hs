{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import Control.Exception
import Test.HUnit

import KnotTheory.PD
import KnotTheory.NamedKnots
import KnotTheory.MetaHopf

assertException :: (Exception e, Eq e) => String -> e -> IO a -> IO ()
assertException preface expected action = do
  r <- catches
          (action >> return (Just "no exception thrown"))
          [ Handler (\e -> return $ if e == expected
                                       then Nothing
                                       else Just ("wrong exception thrown, expected " ++ show expected ++ ", got: " ++ show e))
          -- see the comments below about these two handlers:
          , Handler (\e -> throw (e :: AsyncException))
          , Handler (\(e::SomeException) -> return $ Just ("some other exception thrown: " ++ show e))
          ]
  case r of
    Nothing -> return ()
    Just msg -> assertFailure (preface ++ ": " ++ msg)

main :: IO Counts
main = runTestTT tests

tests :: Test
tests = TestList [ helperFunctionTests
                 , xingTests
                 , knotObjectTests
                 , knotObjectConversionHelperTests
                 , knotObjectConversionTests
                 , namedKnotsTests
                 , thinPositionTests
                 ]

helperFunctionTests :: Test
helperFunctionTests = "Helper functions" ~: TestList
  [ "mergeBy merges dictionaries with sum" ~: TestList $
      [ mergeBy @_ @Int sum [('i',1),('i',2),('j',3)] ~?= [('i',3),('j',3)]
      , mergeBy @_ @Int sum [('i',1),('i',2),('j',3),('i',-3)] ~?=
          [('i',0),('j',3)]
      , mergeBy @Int @Int sum [] ~?= []
      ]
  , "mergeBy merges dictionaries with concat" ~:
      mergeBy @Int @[Int] concat [(1,[1,11,111]),(2,[2,22,222]),(1,[-1,-11])] ~?=
        [(1,[1,11,111,-1,-11]),(2,[2,22,222])]
  ]

xingTests :: Test
xingTests = "Xing properties" ~: TestList
  [ "Xp isPositive?" ~:
      isPositive (Xp @Int 1 2)  ~?= True
  , "Xm is Positive?" ~:
      isPositive (Xm @Int 1 2) ~?= False
  , "Xp is Negative?" ~:
      isNegative (Xp @Int 1 2)  ~?= False
  , "Xm is Negative?" ~:
      isNegative (Xm @Int 1 2)  ~?= True
  -- , "Xv is Positive?" ~:
      -- (isPositive (Xv 1 2)) ~?= False
  -- , "Xv is Negative?" ~:
      -- (isNegative (Xv 1 2)) ~?= False
  , "Sign of Xp" ~:
      sign @Int (Xp 1 2 :: Xing Int) ~?= 1
  , "Sign of Xp" ~:
      sign @Int (Xm 1 2) ~?= -1
  -- , "Sign of Xv" ~:
      -- sign (Xv 1 2) ~?= 0
  , "otherArc produces the other arc" ~: TestList
      [ otherArc (Xp 1 2) 1 ~?= Just @Int 2
      , otherArc (Xp 1 2) 2 ~?= Just @Int 1
      , otherArc (Xp 1 2) 3 ~?= Nothing @Int
      , otherArc (Xm 1 2) 1 ~?= Just @Int 2
      , otherArc (Xm 1 2) 2 ~?= Just @Int 1
      , otherArc (Xm 1 2) 3 ~?= Nothing @Int
      ]
  , "getXingIndices returns list of indices connected to the xing" ~: TestList $
      let
        s = [Strand[1,2],Loop[3,4]]
      in
         [ getXingIndices s (Xp 1 3 :: Xing Int) ~?= [1,3,2,4]
         , getXingIndices s (Xp 2 3) ~?= [2,3,4]
         , getXingIndices s (Xp 2 4) ~?= [2,4,3]
         , getXingIndices s (Xp 1 4) ~?= [1,4,2,3]
         ]
  ]

knotObjectTests :: Test
knotObjectTests = "KnotObject operations" ~: TestList
  [ "next works on finite list" ~:
      next 1 [1,2] ~?= Just @Int 2
  , "next fails at end of list" ~:
      next 2 [1,2] ~?= Nothing @Int
  , "next fails with invalid index" ~:
      next 3 [1,2] ~?= Nothing @Int
  , "nextCyc works on finite list" ~:
      nextCyc 1 [1,2] ~?= Just @Int 2
  , "nextCyc works at end of list" ~:
      nextCyc 2 [1,2] ~?= Just @Int 1
  , "nextCyc fails with invalid index" ~:
      nextCyc 3 [1,2] ~?= Nothing @Int
  , "nextComponentIndex behaves well at end of Strand" ~:
      nextComponentIndex 2 (Strand @Int [1,2]) ~?= Nothing
  , "nextComponentIndex behaves well at 'end' of Loop" ~:
      nextComponentIndex 2 (Loop @Int [1,2]) ~?= Just 1
  , "nextSkeletonIndex works on strand" ~:
      nextSkeletonIndex (skeleton $ SX @Int [Strand [1,2], Loop [3,4]] []) 1 ~?= Just 2
  , "nextSkeletonIndex fails on end of strand" ~:
      nextSkeletonIndex (skeleton $ SX @Int [Strand [1,2], Loop [3,4]] []) 2 ~?= Nothing
  , "nextSkeletonIndex passes on end of loop after first strand" ~:
      nextSkeletonIndex (skeleton $ SX @Int [Strand [1,2], Loop [3,4]] []) 4 ~?= Just 3
  , "prev works on finite list" ~:
      prev 2 [1,2] ~?= Just @Int 1
  , "prev fails at end of list" ~:
      prev 1 [1,2] ~?= Nothing @Int
  , "prev fails with invalid index" ~:
      prev 3 [1,2] ~?= Nothing @Int
  , "prevCyc works on finite list" ~:
      prevCyc 2 [1,2] ~?= Just @Int 1
  , "prevCyc works at end of list" ~:
      prevCyc 1 [1,2] ~?= Just @Int 2
  , "prevCyc fails with invalid index" ~:
      prevCyc 3 [1,2] ~?= Nothing @Int
  , "prevComponentIndex behaves well at beginning of Strand" ~:
      prevComponentIndex 1 (Strand [1,2]) ~?= Nothing @Int
  , "prevComponentIndex behaves well at 'beginning' of Loop" ~:
      prevComponentIndex 1 (Loop [1,2]) ~?= Just @Int 2
  , "prevSkeletonIndex works on strand" ~:
      prevSkeletonIndex (skeleton $ SX [Strand [1,2], Loop [3,4]] []) 2 ~?= Just @Int 1
  , "prevSkeletonIndex fails on beginning of strand" ~:
      prevSkeletonIndex (skeleton $ SX [Strand [1,2], Loop [3,4]] []) 1 ~?= Nothing @Int
  , "prevSkeletonIndex passes on end of loop after first strand" ~:
      prevSkeletonIndex (skeleton $ SX [Strand [1,2], Loop [3,4]] []) 3 ~?= Just @Int 4
  , "isHeadOf passes for head" ~:
      isHeadOf @Int 1 [1,2] ~?= True
  , "isHeadOf fails for nonhead" ~:
      isHeadOf @Int 2 [1,2] ~?= False
  , "isHeadOf fails for nonelement" ~:
      isHeadOf @Int 3 [1,2] ~?= False
  , "isLastOf passes for last" ~:
      isLastOf @Int 2 [1,2] ~?= True
  , "isLastOf fails for nonlast" ~:
      isLastOf @Int 1 [1,2] ~?= False
  , "isLastOf fails for nonelement" ~:
      isLastOf @Int 3 [1,2] ~?= False
  , "isHeadOfComponent passes for head" ~:
      isHeadOfComponent 1 (Strand @Int [1,2]) ~?= True
  , "isHeadOfComponent fails for nonhead" ~:
      isHeadOfComponent 2 (Strand @Int [1,2]) ~?= False
  , "isHeadOfComponent fails for nonelement" ~:
      isHeadOfComponent 3 (Strand @Int [1,2]) ~?= False
  , "isHeadOfComponent passes for head" ~:
      isHeadOfComponent 1 (Loop @Int [1,2]) ~?= False
  , "isHeadOfComponent fails for nonhead" ~:
      isHeadOfComponent 2 (Loop @Int [1,2]) ~?= False
  , "isHeadOfComponent fails for nonelement" ~:
      isHeadOfComponent 3 (Loop @Int [1,2]) ~?= False
  , "isLastOfComponent passes for last" ~:
      isLastOfComponent 2 (Strand @Int [1,2]) ~?= True
  , "isLastOfComponent fails for nonlast" ~:
      isLastOfComponent 1 (Strand @Int [1,2]) ~?= False
  , "isLastOfComponent fails for nonelement" ~:
      isLastOfComponent 3 (Strand @Int [1,2]) ~?= False
  , "isLastOfComponent passes for last" ~:
      isLastOfComponent 2 (Loop @Int [1,2]) ~?= False
  , "isLastOfComponent fails for nonlast" ~:
      isLastOfComponent 1 (Loop @Int [1,2]) ~?= False
  , "isLastOfComponent fails for nonelement" ~:
      isLastOfComponent 3 (Loop @Int [1,2]) ~?= False
  , "findNextXing finds next Xing" ~: TestList $
      zipWith
        (\d x -> findNextXing
          (SX @Int [Loop [1,2,3,4,5,6]] [Xp 1 4, Xp 5 2, Xp 3 6]) d
          ~?= Just x
        )
        [(1,Out), (2,Out), (3,In), (6,Out), (1,In)]
        [Xp 1 4 , Xp 5 2 , Xp 5 2, Xp 3 6 , Xp 3 6]
  , "findNextXing fails to find next Xing" ~: TestList $
    map
      (let k = SX @Int [Strand [1,2,3,4,5,6]] [Xp 1 4, Xp 5 2, Xp 3 6]
        in \d -> findNextXing k d ~?= Nothing
      )
    [(1,In), (7,In), (7,Out)]
  ]

testRVTs :: [RVT Int]
testRVTs = [ RVT
               [Strand [1]]
               []
               []
           , RVT --  trefoil
               [Strand [1,2,3,4,5,6]]
               [Xp 1 4, Xp 5 2, Xp 3 6]
               [(4,-1)]
           , RVT
               [Strand [1,3], Loop [2,4]]
               [Xp 1 2, Xp 4 3]
               [(2,-1)]
           , RVT
               [Strand [1,2,3,4], Loop [5,6,7,8,9,10]]
               [Xm 1 6,Xm 7 10,Xm 5 4,Xp 9 2,Xp 3 8]
               [(3,1),(4,-1),(6,1),(8,1)]
           ]

testSXs :: [SX Int]
testSXs = [ SX
               [Strand [1]]
               []
           , SX -- (closed) trefoil
               [Strand [1,2,3,4,5,6]]
               [Xp 1 4, Xp 5 2, Xp 3 6]
           , SX
               [Strand [1,3], Loop [2,4]]
               [Xp 1 2, Xp 4 3]
           , SX
               [Strand [1,2,3,4],Loop [5,6,7,8,9,10]]
               [Xm 1 6,Xm 7 10,Xm 5 4,Xp 9 2,Xp 3 8]
           ]

knotObjectConversionHelperTests :: Test
knotObjectConversionHelperTests = "KnotObject conversion helper functions" ~: TestList
  [ "absorbXing absorbs xing attached to first element of front" ~: TestList $
    [ TestList $ zipWith3
        (let k = SX @Int [Loop [1,2,3,4,5,6]] [Xp 1 4, Xp 5 2, Xp 3 6]
          in \f x rf -> absorbXing k x f ~?= rf
        )
        (map return [(1,Out), (1,In), (2,Out),(2, In)])
        [Xp 1 4, Xp 3 6, Xp 5 2, Xp 1 4]
        [ ([]     ,[(5,Out),(2,Out),(4,In )])
        , ([]     ,[(4,Out),(6,In ),(3,In )])
        , ([(5,1)],[(5,In ),(3,Out),(6,Out)])
        , ([(5,1)],[(4,In ),(1,In ),(5,Out)])
        ]
    , TestList $ zipWith3
        (let k = SX @Int [Loop [1,2,3,4,5,6]] [Xm 4 1, Xm 2 5, Xm 6 3]
          in \f x rf -> absorbXing k x f ~?= rf
        )
        (map return [(1,Out), (1,In), (2,Out),(2, In)])
        [Xm 4 1, Xm 6 3, Xm 2 5, Xm 4 1]
        [ ([]     ,[(5,Out),(2,Out),(4,In )])
        , ([]     ,[(4,Out),(6,In ),(3,In )])
        , ([(5,1)],[(5,In ),(3,Out),(6,Out)])
        , ([(5,1)],[(4,In ),(1,In ),(5,Out)])
        ]
    ]
  , "advanceFront works when xing present" ~: TestList $
    zipWith
      (let k = SX @Int [Loop [1,2,3,4,5,6]] [Xp 1 4, Xp 5 2, Xp 3 6]
        in \f rf -> advanceFront k f ~?= rf
      )
      (map return [(1,Out), (1,In), (2,Out)])
      [ ([]     ,[(5,Out),(2,Out),(4,In )])
      , ([]     ,[(4,Out),(6,In ),(3,In )])
      , ([(5,1)],[(5,In ),(3,Out),(6,Out)])
      ]
  , "(>>= advanceFront) chains properly" ~: TestList $
      [ "(>>= advanceFront)" ~: TestList $ zipWith
          ( \f rf ->
            let k = SX @Int [Loop [1,2,3,4,5,6]] [Xp 1 4, Xp 5 2, Xp 3 6]
             in (f >>= advanceFront k) ~?= rf
          )
          [ ([(1,1)],[(1,Out)])
          , ([(1,1)],[(2,Out),(1,In),(1,Out),(2,In)])
          ]
          [ ([(1,1)],[(5,Out),(2,Out),(4,In)])
          , ([(1,1),(2,-1)],[(1,In),(1,Out)])
          ]
      , "(>>= advanceFront)^○2" ~: TestList $ zipWith
          ( \f rf ->
            let k = SX @Int [Strand [1,2,3,4,5,6]] [Xp 1 4, Xp 5 2, Xp 3 6]
             in (f >>= advanceFront k >>= advanceFront k) ~?= rf
          )
          [ ([(1,1)],[(1,In),(1,Out)])
          , ([(1,1)],[(2,Out),(1,In),(1,Out),(2,In)])
          , ([(1,1)],[(1,In),(2,Out),(1,Out),(2,In)])
          ]
          [ ([(1,1)],[])
          , ([(1,1),(2,-1)],[])
          ]
      , "(>>= advanceFront) Hopf link" ~: TestList $ zipWith
          ( \f rf ->
            let k = SX @Int [Strand [1,3], Loop [2,4]] [Xp 1 2, Xp 4 3]
             in (f >>= advanceFront k) ~?= rf
          )
          [ ([(1,1)],[(1,Out)])
          , ([(2,1)],[(2,Out)])
          ]
          [ ([(1,1)],      [(4,Out),(3,Out),(2,In)])
          , ([(2,1),(1,1)],[(1,In),(4,Out),(3,Out)])
          ]
      , "(>>= advanceFront)^n (Hopf link)" ~: TestList $
          zipWith (~?=)
            ( take 6 $ iterate
                ( let k = SX @Int [Strand [1,3], Loop [2,4]] [Xp 1 2, Xp 4 3]
                   in (>>= advanceFront k)
                )
                ([(1,1)],[(1,Out)])
            )
            [ ([(1,1)],[(1,Out)])
            , ([(1,1)],[(4,Out),(3,Out),(2,In)])
            , ([(1,1)],[(2,Out),(3,In),(3,Out),(2,In)])
            , ([(1,1),(2,-1)],[(3,In),(3,Out)])
            , ([(1,1),(2,-1)],[])
            , ([(1,1),(2,-1)],[])
            ]
    ]
  , "advanceFront works when arc connects back to front" ~: TestList $
      [ TestList $ zipWith
          (let k = SX @Int [Loop [1]] []
            in \f rf -> advanceFront k f ~?= rf
          )
          [ [(1,Out), (1,In )]
          , [(1,In ), (1,Out)]
          , [(2,Out), (1, In), (2,In ), (3, Out)]
          , [(2,In ), (1, In), (2,Out), (3, Out)]
          ]
          [ ([(1,-1)] ,[])
          , ([]       ,[])
          , ([(2,-1)] ,[(1,In), (3, Out)])
          , ([]       ,[(1,In), (3, Out)])
          ]
      , TestList $ zipWith
          (let k = SX @Int [Loop [1,2,3,4,5,6]] [Xp 1 4, Xp 5 2, Xp 3 6]
            in \f rf -> advanceFront k f ~?= rf
          )
          [ [(1,Out), (1,In )]
          , [(1,In ), (1,Out)]
          , [(2,Out), (1, In), (2,In ), (3, Out)]
          ]
          [ ([(1,-1)] ,[])
          , ([]       ,[])
          , ([(2,-1)] ,[(1,In), (3, Out)])
          ]
      ]
  , "getRotNums tests" ~: TestList
      [ getRotNums (SX @Int [Loop [1]] []) [(1,In ),(1,Out)] ~?= []
      , getRotNums (SX @Int [Loop [1]] []) [(1,Out),(1,In )] ~?= [(1,-1)]
      ]
  ]

-- NB: Conversion program cycles are expected to produce an equivalent knot
-- diagram, not and identical knot program. Keep this in mind if a test fails.
knotObjectConversionTests :: Test
knotObjectConversionTests = "KnotObject conversions" ~: TestList
  [ "toSX . toRVT == id" ~: TestList
    [ TestList $ map (\s -> (toSX . toRVT) s ~?= s) testSXs
    , TestList $ map (\s -> (toSX . toRVT) s ~?= s) allKnots
    , TestList $ map (\s -> (toSX . toRVT) s ~?= s) allLinks
    ]
  , "toRVT . toSX == id" ~: TestList $
      map (\r -> (toRVT . toSX) r ~?= r) testRVTs
  , "toSX is idempotent" ~: TestList
      [ "toSX . toSX $ SX" ~: TestList $
          map (\s -> (toSX . toSX) s ~?= toSX s) testSXs
      , "toSX . toSX $ RVT" ~: TestList $
          map (\r -> (toSX . toSX) r ~?= toSX r) testRVTs
      ]
  , "toRVT is idempotent" ~: TestList
      [ "toRVT . toRVT $ RVT" ~: TestList $
          map (\r -> (toRVT . toRVT) r ~?= toRVT r) testRVTs
      , "toRVT . toRVT $ SX" ~: TestList $
          map (\s -> (toRVT . toRVT) s ~?= toRVT s) testSXs
      ]
  , "toRVT returns classical kinks" ~: TestList $
    [ TestList $ map (\s -> let rs = map (rotnum . toRVT $ s) .
                                    strandIndices . skeleton $ s
                                m = minimum rs
       in -1 <= m ~? "nonusual negative kink detected:" ++ show m
                     ) testSXs
    , TestList $ map (\s -> let rs = map (rotnum . toRVT $ s) .
                                    strandIndices . skeleton $ s
        in let m = maximum rs
            in m <= 1 ~? "nonusual positive kink detected:" ++ show m
                     ) testSXs
    ]
  , "toSX does not change xing data of RVT" ~: TestList $
    [ TestList $
        map (\r -> skeleton r == (skeleton . toSX) r ~? "skeletons do not match"
            ) testRVTs
    , TestList $
        map (\r -> xings r == (xings . toSX) r ~? "xings do not match"
            ) testRVTs
    ]
  , "toRVT does not change xing data of SX" ~: TestList $
    [ TestList $
        map (\s -> (skeleton . toRVT) s == skeleton s ~? "skeletons do not match"
            ) testSXs
    , TestList $
      map (\s -> (xings . toRVT) s == xings s ~? "xings do not match"
          ) testSXs
    ]
  -- , "toRVT does not add a rotation number to terminal arcs" ~: TestList $
      -- map (\s -> let
          -- r = toRVT s
          -- rs = rotnums r

          -- getEndpointsOfSkeleton :: Skeleton a -> [a]
          -- getEndpointsOfSkeleton = concat . map getEndpointsOfComponent

          -- getEndpointsOfComponent :: Component a -> [a]
          -- getEndpointsOfComponent (Loop   _ ) = []
          -- getEndpointsOfComponent (Strand is) = [head is, last is]
                  -- in True ~=? (and $ map ((== Just 0) . flip lookup rs) (getEndpointsOfSkeleton . skeleton $ r))
          -- ) testSXs
  ]

namedKnotsTests :: Test
namedKnotsTests = "Named knots/ links tests" ~: TestList
  [ "Named knot lookup makes sense" ~:
      knot 3 True 1 ~?= SX [Loop[1, 2, 3, 4, 5, 6]] [Xm 4 1, Xm 6 3, Xm 2 5]
  , "Named knot fails appropriately" ~:
      assertException "invalid knot" InvalidNamedKnot (evaluate $ knot 3 True (-1))
  , "Named link lookup makes sense" ~:
      link 2 True 1 ~?= SX [Loop[1, 2], Loop[3, 4]] [Xm 1 4, Xm 3 2]
  , "Named link fails appropriately" ~:
      assertException "invalid link" InvalidNamedLink (evaluate $ link 2 True (-1))
  ]

metaHopfTests :: Test
metaHopfTests = "Meta-Hopf algebra tests" ~: TestList
  [ toMetaHopfExpression (RVT [Loop[1 :: Int]] [] [(1,0)]) ~?= [Unit 1]
  ]

thinPositionTests :: Test
thinPositionTests = "Thin position tests" ~: TestList
        [ "width of a knot returns a correct value." ~:
                width (knot 3 True 1) ~?= 4
        , "thinPosition of a knot becomes shorter" ~:
                (width . thinPosition) (knot 8 True 1) ~?= 4
        ]

tangleTests :: Test
tangleTests = "Tangle-manipulation tests" ~: TestList
        [ "applyAt applies a function at the appropriate place on a list" ~:
                applyAt negate 3 [0,1,2,-3] ~?= [0..3::Int] 
        , "diag generates a diagonal matrix" ~:
                diag (const @Int 1) (replicate 3 0) ~?= [[1,0,0],[0,1,0],[0,0,1]]
        , "cycles produces all cycles of a finite list" ~:
                cycles @Int [1,2,3] ~?= [[1,2,3],[2,3,1],[3,1,2]]
        , "cycles behaves appropriately on an empty list" ~:
                cycles @[Int] [] ~?= [[]]
        , "compCuts returns all possible strands from a component" ~:
                compCuts (Loop @Int [1,2]) [Loop[3,4],Loop[1,2]] ~?=
                        [ [Strand[1,2],Loop[3,4]]
                        , [Strand[1,2],Loop[3,4]]
                        ]
        ]
