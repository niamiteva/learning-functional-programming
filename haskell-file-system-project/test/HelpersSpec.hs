{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HelpersSpec
  ( helpersSpec
  ) where

import HsUnix

helpersSpec :: Spec
helpersSpec = do
  wordsBySpec

wordsBySpec :: Spec
wordsBySpec = describe "wordsBy" do
  it "Works on an example" $
    '/' "hello/word/alabala"
    `shouldBe`
    ["hello", "word", "alabala"]

