module Main where

import Criterion
import Criterion.Main

import Distribution.Version as IUT
import Control.DeepSeq

v :: [Int] -> [Int]
v = id

main :: IO ()
main = defaultMain
    [ bench "maximum [1,2,3,4]"               $ whnf maximum (v [1,2,3,4]) -- base-line
    , bench "rnf [1,2,3,4]"                   $ whnf rnf (v [1,2,3,4]) -- base-line

{-
    , bench "versionNumbers 1.2"                   $ nf versionNumbers (mkVersion [1,2])
    , bench "versionNumbers 1.2.3.4"               $ nf versionNumbers (mkVersion [1,2,3,4])
    , bench "versionNumbers 1.2.3.4.5"             $ nf versionNumbers (mkVersion [1,2,3,4,5])

    , bench "versionNumbersLazy 1.2"                   $ nf versionNumbersLazy (mkVersion [1,2])
    , bench "versionNumbersLazy 1.2.3.4"               $ nf versionNumbersLazy (mkVersion [1,2,3,4])
    , bench "versionNumbersLazy 1.2.3.4.5"             $ nf versionNumbersLazy (mkVersion [1,2,3,4,5])

    , bench "take2 1.2.3.4"                   $ whnf (alterVersion (take 2)) (mkVersion [1,2,3,4])

    , bench "(==) 1.2.3.4 1.2.3.4"            $ whnf ((==) (mkVersion [1,2,3,4])) (mkVersion [1,2,3,4])
    , bench "(==) [1.2.3.4] [1.2.3.4]"        $ whnf ((==) (v [1,2,3,4])) (v [1,2,3,4])

    , bench "(==) 1.2.3.4.5 1.2.3.4.5"        $ whnf ((==) (mkVersion [1,2,3,4,5])) (mkVersion [1,2,3,4,5])
    , bench "(==) [1.2.3.4.5] [1.2.3.4.5]"    $ whnf ((==) (v [1,2,3,4,5])) (v [1,2,3,4,5])

    , bench "compare 1.2.3.4 1.2.3.4"         $ whnf (compare (mkVersion [1,2,3,4])) (mkVersion [1,2,3,4])
    , bench "compare [1.2.3.4] [1.2.3.4]"     $ whnf (compare (v [1,2,3,4])) (v [1,2,3,4])

    , bench "compare 1.2.3.4.5 1.2.3.4.5"     $ whnf (compare (mkVersion [1,2,3,4,5])) (mkVersion [1,2,3,4,5])
    , bench "cmpLazy 1.2.3.4.5 1.2.3.4.5"     $ whnf (cmpLazy (mkVersion [1,2,3,4,5])) (mkVersion [1,2,3,4,5])
    , bench "cmpOpt  1.2.3.4.5 1.2.3.4.5"     $ whnf (cmpOpt  (mkVersion [1,2,3,4,5])) (mkVersion [1,2,3,4,5])
    , bench "compare [1.2.3.4.5] [1.2.3.4.5]" $ whnf (compare (v [1,2,3,4,5])) (v [1,2,3,4,5])

    , bench "compare 2.3.4 1.2.3.4.5"     $ whnf (compare (mkVersion [2,3,4])) (mkVersion [1,2,3,4,5])
    , bench "cmpLazy 2.3.4 1.2.3.4.5"     $ whnf (cmpLazy (mkVersion [2,3,4])) (mkVersion [1,2,3,4,5])
    , bench "cmpOpt  2.3.4 1.2.3.4.5"     $ whnf (cmpOpt  (mkVersion [2,3,4])) (mkVersion [1,2,3,4,5])
    , bench "compare [2.3.4] [1.2.3.4.5]" $ whnf (compare (v [2,3,4])) (v [1,2,3,4,5])

-}

    , mkV [1]
    , mkV [1,2]
    , mkV [1,2,3]
    , mkV [1,2,3,4]
    , mkV [1,2,3,4,5]
    , mkV [20160101,2,3,4]
    , mkV [1,2,3,20160101]

    ]
    -- putStrLn "Hello, Haskell!"
  where
    mkV vs = bgroup (show vs)
        [ bench "mkVersion0"  $ whnf mkVersion0 vs
        , bench "mkVersion"   $ whnf mkVersion  vs
        , bench "mkVersion1"  $ whnf mkVersion1 vs
        , bench "mkVersion2"  $ whnf mkVersion2 vs
        , bench "mkVersion3"  $ whnf mkVersion3 vs
        ]
