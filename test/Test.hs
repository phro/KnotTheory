{-# LANGUAGE ScopedTypeVariables #-}
module Test where
import Control.Exception
import Control.Monad
import Test.HUnit

import KnotTheory

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

main = runTestTT tests
tests :: Test
tests = TestList [ helperFunctionTests
                 , xingTests
                 , knotObjectTests
                 , knotObjectConversionTests
                 ]

helperFunctionTests = "Helper functions" ~: TestList
  [ "mergeBy merges dictionaries with sum" ~:
      mergeBy sum [('i',1),('i',2),('j',3)] ~?= [('i',3),('j',3)]
  ]

xingTests = "Xing properties" ~: TestList
  [ "Xp isPositive?" ~:
      (isPositive (Xp 1 2)) ~?= True
  , "Xm is Positive?" ~:
      (isPositive (Xm 1 2)) ~?= False
  , "Xp is Negative?" ~:
      (isNegative (Xp 1 2)) ~?= False
  , "Xm is Negative?" ~:
      (isNegative (Xm 1 2)) ~?= True
  -- , "Xv is Positive?" ~:
      -- (isPositive (Xv 1 2)) ~?= False
  -- , "Xv is Negative?" ~:
      -- (isNegative (Xv 1 2)) ~?= False
  , "Sign of Xp" ~:
      sign (Xp 1 2) ~?= 1
  , "Sign of Xp" ~:
      sign (Xm 1 2) ~?= -1
  -- , "Sign of Xv" ~:
      -- sign (Xv 1 2) ~?= 0
  ]

knotObjectTests = "KnotObject operations" ~: TestList
  [ "next works on finite list" ~:
      next 1 [1,2] ~?= Just 2
  , "next fails at end of list" ~:
      next 2 [1,2] ~?= Nothing
  , "next fails with invalid index" ~:
      next 3 [1,2] ~?= Nothing
  , "nextCyc works on finite list" ~:
      nextCyc 1 [1,2] ~?= Just 2
  , "nextCyc works at end of list" ~:
      nextCyc 2 [1,2] ~?= Just 1
  , "nextCyc fails with invalid index" ~:
      nextCyc 3 [1,2] ~?= Nothing
  , "nextComponentIndex behaves well at end of Strand" ~:
      nextComponentIndex 2 (Strand [1,2]) ~?= Nothing
  , "nextComponentIndex behaves well at 'end' of Loop" ~:
      nextComponentIndex 2 (Loop [1,2]) ~?= Just 1
  , "nextSkeletonIndex works on strand" ~:
      nextSkeletonIndex 1 (skeleton $ SX [Strand [1,2], Loop [3,4]] []) ~?= Just 2
  , "nextSkeletonIndex fails on end of strand" ~:
      nextSkeletonIndex 2 (skeleton $ SX [Strand [1,2], Loop [3,4]] []) ~?= Nothing
  , "nextSkeletonIndex passes on end of loop after first strand" ~:
      nextSkeletonIndex 4 (skeleton $ SX [Strand [1,2], Loop [3,4]] []) ~?= Just 3
  , "prev works on finite list" ~:
      prev 2 [1,2] ~?= Just 1
  , "prev fails at end of list" ~:
      prev 1 [1,2] ~?= Nothing
  , "prev fails with invalid index" ~:
      prev 3 [1,2] ~?= Nothing
  , "prevCyc works on finite list" ~:
      prevCyc 2 [1,2] ~?= Just 1
  , "prevCyc works at end of list" ~:
      prevCyc 1 [1,2] ~?= Just 2
  , "prevCyc fails with invalid index" ~:
      prevCyc 3 [1,2] ~?= Nothing
  , "prevComponentIndex behaves well at beginning of Strand" ~:
      prevComponentIndex 1 (Strand [1,2]) ~?= Nothing
  , "prevComponentIndex behaves well at 'beginning' of Loop" ~:
      prevComponentIndex 1 (Loop [1,2]) ~?= Just 2
  , "prevSkeletonIndex works on strand" ~:
      prevSkeletonIndex 2 (skeleton $ SX [Strand [1,2], Loop [3,4]] []) ~?= Just 1
  , "prevSkeletonIndex fails on beginning of strand" ~:
      prevSkeletonIndex 1 (skeleton $ SX [Strand [1,2], Loop [3,4]] []) ~?= Nothing
  , "prevSkeletonIndex passes on end of loop after first strand" ~:
      prevSkeletonIndex 3 (skeleton $ SX [Strand [1,2], Loop [3,4]] []) ~?= Just 4
  , "isHeadOf passes for head" ~:
      isHeadOf 1 [1,2] ~?= True
  , "isHeadOf fails for nonhead" ~:
      isHeadOf 2 [1,2] ~?= False
  , "isHeadOf fails for nonelement" ~:
      isHeadOf 3 [1,2] ~?= False
  , "isLastOf passes for last" ~:
      isLastOf 2 [1,2] ~?= True
  , "isLastOf fails for nonlast" ~:
      isLastOf 1 [1,2] ~?= False
  , "isLastOf fails for nonelement" ~:
      isLastOf 3 [1,2] ~?= False
  , "isHeadOfComponent passes for head" ~:
      isHeadOfComponent 1 (Strand [1,2]) ~?= True
  , "isHeadOfComponent fails for nonhead" ~:
      isHeadOfComponent 2 (Strand [1,2]) ~?= False
  , "isHeadOfComponent fails for nonelement" ~:
      isHeadOfComponent 3 (Strand [1,2]) ~?= False
  , "isHeadOfComponent passes for head" ~:
      isHeadOfComponent 1 (Loop [1,2]) ~?= False
  , "isHeadOfComponent fails for nonhead" ~:
      isHeadOfComponent 2 (Loop [1,2]) ~?= False
  , "isHeadOfComponent fails for nonelement" ~:
      isHeadOfComponent 3 (Loop [1,2]) ~?= False
  , "isLastOfComponent passes for last" ~:
      isLastOfComponent 2 (Strand [1,2]) ~?= True
  , "isLastOfComponent fails for nonlast" ~:
      isLastOfComponent 1 (Strand [1,2]) ~?= False
  , "isLastOfComponent fails for nonelement" ~:
      isLastOfComponent 3 (Strand [1,2]) ~?= False
  , "isLastOfComponent passes for last" ~:
      isLastOfComponent 2 (Loop [1,2]) ~?= False
  , "isLastOfComponent fails for nonlast" ~:
      isLastOfComponent 1 (Loop [1,2]) ~?= False
  , "isLastOfComponent fails for nonelement" ~:
      isLastOfComponent 3 (Loop [1,2]) ~?= False
  , "findNextXing finds next Xing" ~: TestList
      zipWith
        (\d x -> findNextXing
          (SX [Loop [1,2,3,4,5,6]] [Xp 1 4, Xp 5 2, Xp 3 6]) d
          ~?= Just x
        )
                  [(1,Out), (2,Out), (3,In), (6,Out), (1,In)]
        (map Just [Xp 1 4 , Xp 2 5 , Xp 5 2, Xp 3 6 , Xp 3 6])
      )
  ]

testRVTs :: [KnotObject Int]
testRVTs = [ RVT -- (long) trefoil TODO: verify
               [Strand [1,2,3,4,5,6]]
               [Xp 1 4, Xp 5 2, Xp 3 6]
               [(1,0),(2,0),(3,0),(4,-1),(5,0),(6,0)]
           , RVT -- (closed) trefoil TODO: verify
               [Loop [1,2,3,4,5,6]]
               [Xp 1 4, Xp 5 2, Xp 3 6]
               [(1,1),(2,0),(3,0),(4,-1),(5,0),(6,0)]
           ]

testSXs = [ SX -- (long) trefoil TODO: verify
               [Strand [1,2,3,4,5,6]]
               [Xp 1 4, Xp 5 2, Xp 3 6]
           , SX -- (closed) trefoil TODO: verify
               [Loop [1,2,3,4,5,6]]
               [Xp 1 4, Xp 5 2, Xp 3 6]
           ]

-- NB: Conversion program cycles are expected to produce an equivalent knot
-- diagram, not and identical knot program. Keep this in mind if a test fails.
knotObjectConversionTests = "KnotObject conversions" ~: TestList
  [ "isSX reads KnotObjects properly" ~: TestList $
      [ TestList $ map (\s -> isSX s ~?= True) testSXs
      , TestList $ map (\r -> isSX r ~?= False) testRVTs
      ]
  , "isRVT reads KnotObjects properly" ~: TestList $
      [ TestList $ map (\r -> isRVT r ~?= True) testRVTs
      , TestList $ map (\s -> isRVT s ~?= False) testSXs
      ]
  , "toSX returns SX" ~: TestList $
      map (\s -> (isSX . toSX) s ~? "toSX did not return an SX") $ testSXs++testRVTs
  , "toRVT returns RVT" ~: TestList $
      map (\s -> (isRVT . toRVT) s ~? "toRVT did not return an RVT") $ testSXs++testRVTs
  , "toSX . toRVT == id" ~: TestList $
      map (\s -> (toSX . toRVT) s ~?= s) testSXs
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
    [ TestList $ map (\s -> let rs = map snd . rotnums $ s
        in let m = minimum rs
            in -1 <= m ~? "nonusual negative kink detected:" ++ show m
                     ) testSXs
    , TestList $ map (\s -> let rs = map snd . rotnums $ s
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
  , "toRVT does not add a rotation number to terminal arcs" ~: TestList $
      map (\s -> let
          r = toRVT s
          rs = rotnums r

          getEndpointsOfSkeleton :: Skeleton a -> [a]
          getEndpointsOfSkeleton = concat . map getEndpointsOfComponent

          getEndpointsOfComponent :: Component a -> [a]
          getEndpointsOfComponent (Loop   _ ) = []
          getEndpointsOfComponent (Strand is) = [head is, last is]
                  in True ~=? (and $ map ((== Just 0) . flip lookup rs) (getEndpointsOfSkeleton . skeleton $ r))
          ) testSXs
  ]
