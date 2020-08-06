module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "hello" $ do
    it "returns 'hello world!'" $ do
      hello "world" `shouldBe` "hello world!"

hello :: String -> String
hello name =
  "hello " ++ name ++ "!"
