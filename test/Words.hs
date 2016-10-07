{-# LANGUAGE OverloadedStrings #-}
module Words (tests) where

import qualified Sanskell.Words as W (wordCount)
import qualified Data.Map as M
import Test.HUnit

testSpaces = TestCase $ do
  let wordsCount = W.wordCount [ " \t" ]
  assertEqual "empty list," [] (M.elems wordsCount)

testSpacesWithWords = TestCase $ do
  let wordsCount = W.wordCount [ " ashish here " ]
  assertEqual "should not parse english words," [] (M.elems wordsCount)

testSanskritWords = TestCase $ do
  let wordsCount = W.wordCount [ "आरब्धम् उत्तमजनाः ना परितयजन्ति" ]
  assertEqual "should parse sanskrti words," [1, 1, 1, 1] (M.elems wordsCount)

testSanskritWordsMulti = TestCase $ do
  let wordsCount = W.wordCount [ "आरब्धम् उत्तमजनाः ना परितयजन्ति || आरब्धम् उत्तमजनाः ..." ]
  assertEqual "should parse sanskrti words," [2, 2, 1, 1] (M.elems wordsCount)

tests = runTestTT $ TestList [ TestLabel "empty spaces should be ignored" testSpaces
                             , TestLabel "with words and spaces" testSpacesWithWords
                             , TestLabel "with sanskrit words" testSanskritWords
                             , TestLabel "with multi sanskrit words" testSanskritWordsMulti]
