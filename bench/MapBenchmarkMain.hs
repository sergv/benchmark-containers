-- |
-- Module:     MapBenchmarkMain
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -O2 -ddump-simpl -dsuppress-uniques -dsuppress-idinfo -dsuppress-module-prefixes -dsuppress-type-applications -dsuppress-coercions -dppr-cols200 -dsuppress-type-signatures -ddump-to-file #-}

module MapBenchmarkMain (main) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.ByteString (ByteString)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as TBL
import Data.Traversable
import System.Random.Stateful
import Test.Tasty.Bench
import Test.Tasty.Patterns.Printer (printAwkExpr)

import Utils.Containers.Internal.StrictPair

main :: IO ()
main = do
  let bound      = 4096 :: Int
      elems      = zip testKeys values
      elems_even = zip evens evens
      elems_odd  = zip odds odds
      elems_rev  = reverse elems
      testKeys   = [1..bound]
      evens      = [2,4..bound]
      odds       = [1,3..bound]
      values     = [1..bound]

  let testM       = M.fromAscList elems :: M.Map Int Int
      m_even      = M.fromAscList elems_even :: M.Map Int Int
      m_odd       = M.fromAscList elems_odd :: M.Map Int Int
      m_odd_keys  = M.keysSet m_odd
      m_even_keys = M.keysSet m_even

  evaluate $ rnf [testM, m_even, m_odd]
  evaluate $ rnf elems_rev
  evaluate $ rnf [m_odd_keys, m_even_keys]

  -- let names = ["foo", "baar", "frobnicator", "quux", "decombobulator"]

  let generate :: Int -> IOGenM StdGen -> IO [(ByteString, Int)]
      generate n g = replicateM n $ do
        size <- uniformRM (1 :: Int, 100) g
        name <- uniformByteStringM size g
        val  <- uniformRM (1 :: Int, n) g
        pure (name, val)

  let sizes :: [Int]
      sizes = [1, 10, 100, 1000, 10_000, 100_000, 1_000_000]

  gen <- newIOGenM $ mkStdGen 1

  tests <- for sizes $ \n -> do
    xs <- generate n gen
    let ys = M.fromList xs
    evaluate $ rnf ys
    fmap (ys, ) $ for (filter (<= n) sizes) $ \k -> do
      let subset
            | k == n    = take (k - 1) xs
            | otherwise = take k xs
      evaluate $ force $ S.fromList $ map fst subset

  let restrictAndWithoutBaselineName :: String
      restrictAndWithoutBaselineName = "restrictKeys+withoutKeys"

  defaultMain $
    [ bgroup "partitionKeys"
      [ mapLeafBenchmarks (addCompare restrictAndWithoutBaselineName) $ bgroup "even"
        [ bench restrictAndWithoutBaselineName
        $ whnf (\ks -> M.restrictKeys testM ks :*: M.withoutKeys testM ks) m_even_keys
        , bcompare "/even.restrictKeys+withoutKeys/"
        $ bench "partitionKeys"
        $ whnf (M.partitionKeys testM) m_even_keys
        , bcompare "/even.restrictKeys+withoutKeys/"
        $ bench "partitionWithKey"
        $ whnf (\ks -> M.partitionWithKey (\k _ -> S.member k ks) testM) m_even_keys
        ]
      , mapLeafBenchmarks (addCompare restrictAndWithoutBaselineName) $ bgroup "odd"
        [ bench restrictAndWithoutBaselineName
        $ whnf (\ks -> M.restrictKeys testM ks :*: M.withoutKeys testM ks) m_odd_keys
        , bcompare "/odd.restrictKeys+withoutKeys/"
        $ bench "partitionKeys"
        $ whnf (M.partitionKeys testM) m_odd_keys
        , bcompare "/odd.restrictKeys+withoutKeys/"
        $ bench "partitionWithKey"
        $ whnf (\ks -> M.partitionWithKey (\k _ -> S.member k ks) testM) m_even_keys
        ]
      ]
    ] ++
    [ bgroup group1
      [ mapLeafBenchmarks (addCompare restrictAndWithoutBaselineName) $ bgroup group2
        [ bench restrictAndWithoutBaselineName
        $ whnf (\ks -> M.restrictKeys m ks :*: M.withoutKeys m ks) keys
        , bench "partitionKeys - split set"
        $ whnf (M.partitionKeys m) keys
        , bench "partitionKeys - split map"
        $ whnf (M.partitionKeysSplitMap m) keys
        ]
      | keys <- ss
      , let group2 = "Set " ++ fmt (S.size keys)
      ]
    | (m, ss) <- tests
    , let group1 = "Map " ++ fmt (M.size m)
    ]

fmt :: Int -> String
fmt = T.unpack . formatNumber

addCompare :: String -> [String] -> Benchmark -> Benchmark
addCompare targetBenchName (name : path)
  | name /= targetBenchName
  = bcompare (printAwkExpr (locateBenchmark (targetBenchName : path)))
addCompare _ _ = id

formatNumber :: Int -> Text
formatNumber x = TBL.runBuilder $ sign <> go mempty (abs x)
  where
    sign :: TBL.Builder
    sign = if x < 0 then "-" else ""
    go :: TBL.Builder -> Int -> TBL.Builder
    go acc n
      | n < 1000  = TBL.fromDec n <> acc
      | otherwise = go (TBL.fromChar ',' <> padding <> TBL.fromText k' <> acc) n'
      where
        (n', k) = n `quotRem` 1000
        k'      = TBL.runBuilder $ TBL.fromDec k
        padding = TBL.fromText $ T.replicate (3 - T.length k') $ T.singleton '0'
