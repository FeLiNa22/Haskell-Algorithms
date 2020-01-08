module Tests where

import IC.TestSuite

import Sequences


maxOf2TestCases
  = [ (1, 2) ==> 2,
      (2, 1) ==> 2,
      (2, 2) ==> 2
    ]

maxOf3TestCases
  = [ (1, 2, 3) ==> 3,
      (2, 1, 3) ==> 3,
      (3, 3, 3) ==> 3
    ]

isADigitTestCases
  = [ ('1') ==> True,
      ('A') ==> False
    ]

isAlphaTestCases
  = [ ('1') ==> False,
      ('A') ==> True
    ]

digitToIntTestCases
  = [ ('0') ==> 0,
      ('9') ==> 9
    ]

toUpperTestCases
  = [ ('a') ==> 'A',
      ('A') ==> 'A'
    ]

--
-- Sequences and series
--

arithmeticSeqTestCases
  = [ (0.0, 10.0, 0) ==> 0.0,
      (10.0, 10.0, 0) ==> 10.0,
      (0.0, 10.0, 10) ==> 100.0,
      (10.0, 0.0, 10) ==> 10.0
    ]

geometricSeqTestCases
  = [ (0.0, 10.0, 0) ==> 0.0,
      (10.0, 10.0, 0) ==> 10.0,
      (0.0, 10.0, 10) ==> 0.0,
      (10.0, 0.0, 10) ==> 0.0
    ]

arithmeticSeriesTestCases
  = [ (0.0, 10.0, 0) ==> 0.0,
      (10.0, 10.0, 0) ==> 10.0,
      (0.0, 10.0, 10) ==> 550.0,
      (10.0, 0.0, 10) ==> 110.0
    ]

geometricSeriesTestCases
  = [ (0.0, 10.0, 0) ==> 0.0,
      (10.0, 10.0, 0) ==> 10.0,
      (0.0, 10.0, 10) ==> 0.0,
      (10.0, 0.0, 10) ==> 10.0
    ]

-- You can add your own test cases above

sequencesTestCases
  = [ TestCase  "maxOf2"      (uncurry maxOf2)
                              maxOf2TestCases
     , TestCase "maxOf3"      (uncurry3 maxOf3)
                              maxOf3TestCases
     , TestCase "isADigit"    (isADigit)
                              isADigitTestCases
     , TestCase "isAlpha"     (isAlpha)
                              isAlphaTestCases
     , TestCase "digitToInt"  (digitToInt)
                              digitToIntTestCases
     , TestCase "toUpper"     (toUpper)
                              toUpperTestCases
     , TestCase "arithmeticSeq" (uncurry3 arithmeticSeq)
                                arithmeticSeqTestCases
     , TestCase "geometricSeq"  (uncurry3 geometricSeq)
                                geometricSeqTestCases
     , TestCase "arithmeticSeries"  (uncurry3 arithmeticSeries)
                                    arithmeticSeriesTestCases
     , TestCase "geometricSeries"   (uncurry3 geometricSeries)
                                    geometricSeriesTestCases
    ]

runTests = mapM_ goTest sequencesTestCases

main = runTests
