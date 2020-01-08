module Tests where

import IC.TestSuite

import Crypto

-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption

gcdTestCases
  = [ (0, 0) ==> 0
    , (0, 8) ==> 8
    , (8, 0) ==> 8
    , (3, 3) ==> 3
    , (12, 16) ==> 4
    , (16, 12) ==> 4
    , (65, 40) ==> 5
    , (735, 1239) ==> 21
    ]

phiTestCases
  = [ 0 ==> 0
    , 1 ==> 1
    , 2 ==> 1
    , 6 ==> 2
    , 18 ==> 6
    , 17 ==> 16
    , 31 ==> 30
    , 35 ==> 24
    , 77 ==> 60
    ]

modPowTestCases
  = [ (0, 0, 1) ==> 0
    , (1, 1, 1) ==> 0
    , (1, 1, 2) ==> 1
    , (13481, 11237, 6) ==> 5
    , (8, 0, 1) ==> 0
    , (8, 0, 5) ==> 1
    , (237, 1, 1000) ==> 237
    , (859237, 1, 1000) ==> 237
    , (33893, 2, 10000) ==> 5449
    , (7433893, 2, 10000) ==> 5449
    , (13481503, 11237126, 46340) ==> 6629
    ]

computeCoeffsTestCases
  = [ (0, 0) ==> (1, 0)
    , (0, 8) ==> (0, 1)
    , (12, 16) ==> (-1, 1)
    , (16, 12) ==> (1, -1)
    , (65, 40) ==> (-3, 5)
    , (735, 1239) ==> (27, -16)
    ]

inverseTestCases
  = [ (11, 16) ==> 3
    , (4, 15) ==> 4
    , (18, 35) ==> 2
    , (35, 18) ==> 17
    , (12, 91) ==> 38
    , (34, 91) ==> 83
    , (64, 91) ==> 64
    ]

smallestCoPrimeOfTestCases
  = [ 1 ==> 2
    , 2 ==> 3
    , 12 ==> 5
    , 13 ==> 2
    , 30 ==> 7
    , 210 ==> 11
    ]

genKeysTestCases
  = [ (2, 3) ==> ((3,6),(1,6))
    , (17, 23) ==> ((3,391),(235,391))
    , (101, 83) ==> ((3,8383),(5467,8383))
    , (401, 937) ==> ((7,375737),(213943,375737))
    , (613, 997) ==> ((5,611161),(243821,611161))
    , (26641, 26437) ==> ((7,704308117),(100607863,704308117))
    ]

rsaEncryptTestCases
  = [ (4321, (3,8383)) ==> 3694
    , (324561, (5, 611161)) ==> 133487
    , (1234, (5,611161)) ==> 320878
    , (704308111, (7, 704308117)) ==> 704028181
    ]

rsaDecryptTestCases
  = [ (3694, (5467,8383)) ==> 4321
    , (133487, (243821,611161)) ==> 324561
    , (320878, (243821,611161)) ==> 1234
    , (704028181, (100607863,704308117)) ==> 704308111
    ]

-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

toIntTestCases
  = [ 'a' ==> 0
    , 'z' ==> 25
    , 'h' ==> 7
    ]

toCharTestCases
  = [ 0 ==> 'a'
    , 25 ==> 'z'
    , 7 ==> 'h'
    ]

addTestCases
  = [ ('a', 'a') ==> 'a'
    , ('d', 's') ==> 'v'
    , ('w', 't') ==> 'p'
    ]

substractTestCases
  = [ ('a', 'a') ==> 'a'
    , ('v', 's') ==> 'd'
    , ('p', 'w') ==> 't'
    ]

ecbEncryptTestCases
  = [ ('w', "") ==> ""
    , ('d', "w") ==> "z"
    , ('x', "bonjour") ==> "ylkglro"
    , ('k', "hello") ==> "rovvy"
    ]

ecbDecryptTestCases
  = [ ('w', "") ==> ""
    , ('d', "z") ==> "w"
    , ('x', "ylkglro") ==> "bonjour"
    , ('k', "rovvy") ==> "hello"
    ]

cbcEncryptTestCases
  = [ ('w', 'i', "") ==> ""
    , ('d', 'i', "w") ==> "h"
    , ('x', 'w', "bonjour") ==> "ufpvgxl"
    , ('k', 'q', "hello") ==> "hvqlj"
    ]

cbcDecryptTestCases
  = [ ('w', 'i', "") ==> ""
    , ('d', 'i', "h") ==> "w"
    , ('x', 'w', "ufpvgxl") ==> "bonjour"
    , ('k', 'q', "hvqlj") ==> "hello"
    ]

-- You can add your own test cases above

allTestCases
  = [ TestCase "gcd" (uncurry Crypto.gcd)
                     gcdTestCases
    , TestCase "phi" phi
                     phiTestCases
    , TestCase "modPow" (uncurry3 modPow)
                        modPowTestCases
    , TestCase "computeCoeffs" (uncurry computeCoeffs)
                             computeCoeffsTestCases
    , TestCase "inverse" (uncurry inverse)
                         inverseTestCases
    , TestCase "smallestCoPrimeOf" (smallestCoPrimeOf)
                                   smallestCoPrimeOfTestCases
    , TestCase "genKeys" (uncurry genKeys)
                         genKeysTestCases
    , TestCase "rsaEncrypt" (uncurry rsaEncrypt)
                            rsaEncryptTestCases
    , TestCase "rsaDecrypt" (uncurry rsaDecrypt)
                            rsaDecryptTestCases
    , TestCase "toInt" (toInt)
                       toIntTestCases
    , TestCase "toChar" (toChar)
                       toCharTestCases
    , TestCase "add" (uncurry add)
                     addTestCases
    , TestCase "substract" (uncurry substract)
                           substractTestCases
    , TestCase "ecbEncrypt" (uncurry ecbEncrypt)
                            ecbEncryptTestCases
    , TestCase "ecbDecrypt" (uncurry ecbDecrypt)
                            ecbDecryptTestCases
    , TestCase "cbcEncrypt" (uncurry3 cbcEncrypt)
                            cbcEncryptTestCases
    , TestCase "cbcDecrypt" (uncurry3 cbcDecrypt)
                       cbcDecryptTestCases
    ]


runTests = mapM_ goTest allTestCases

main = runTests
