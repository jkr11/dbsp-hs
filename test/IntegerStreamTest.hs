module Main where

import Test.HUnit
import DBSP.Stream
import Data.Monoid (Sum(..))

main :: IO Counts
main = runTestTT $ TestList
  [ testDelay
  , testDifferentiation
  , testIntegration
  , testNegate
  , testAdd
  ]

testDelay :: Test
testDelay = TestCase $
  let input = Stream [1, 2, 3, 4, 5, 6, 7, 8, 9] :: Stream (Sum Int)
      expected = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  in assertEqual "Delay operator" 
       (fmap Sum expected) 
       (unStream $ delay input)

testDifferentiation :: Test
testDifferentiation = TestCase $
  let input = Stream [0, 1, 3, 6] :: Stream (Sum Int)
      expected = [0, 1, 2, 3]
  in assertEqual "Differentiation" 
       (fmap Sum expected) 
       (unStream $ differentiate input)

testIntegration :: Test
testIntegration = TestCase $
  let input = Stream [0, 1, 1, 1] :: Stream (Sum Int)
      expected = [0, 1, 2, 3]
  in assertEqual "Integration" 
       (fmap Sum expected) 
       (unStream $ integrate input)

testIntegrationIsInverseOfDifferential :: Test
testIntegrationIsInverseOfDifferential = TestCase $
  let input = Stream $ map Sum [0..10] 
      roundTripped = differentiate $ integrate input
  in assertEqual "I(D(s)) == s (prefix)" 
       (take 10 $ unStream input) 
       (take 10 $ unStream roundTripped)

testNegate :: Test
testNegate = TestCase $
  let input = Stream [1, -2, 3] :: Stream (Sum Int)
      expected = [-1, 2, -3]
  in assertEqual "Negation" 
       (fmap Sum expected) 
       (unStream $ fmap (\x -> -1 * x) input)

testAdd :: Test
testAdd = TestCase $
  let s1 = Stream [1, 2, 3] :: Stream (Sum Int)
      s2 = Stream [4, 5, 6] :: Stream (Sum Int)
      expected = [5, 7, 9]
  in assertEqual "Addition" 
       (fmap Sum expected) 
       (unStream $ zipStreams (<>) s1 s2)
