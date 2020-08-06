module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

data Direction = North | East | West | South deriving (Eq)

instance Show Direction where
  show North = "North"
  show East = "East"
  show West = "West"
  show South = "South"

{- |
Removes any needless directions from a list of directions.
We should not go North, then immediately South for instance.
-}
dirReduce :: [Direction] -> [Direction]
dirReduce ds
  | newDs == ds = ds
  | otherwise = dirReduce newDs
  where newDs = dirReduce' ds

dirReduce' :: [Direction] -> [Direction]
dirReduce' [] = []
dirReduce' [d] = [d]
dirReduce' (x:y:ds)
  | elem (x,y) [(North,South),(South,North),(East,West),(West,East)] = dirReduce' ds
  | otherwise = x:(dirReduce' (y:ds))

main :: IO ()
main = hspec $ do
  describe "dirReduce - simple tests" $ do
    it "should work for some small examples" $ do
      dirReduce [     ]             `shouldBe` [     ]
      dirReduce [North]             `shouldBe` [North]
      dirReduce [North, West]       `shouldBe` [North,West]      
      dirReduce [North, West, East] `shouldBe` [North]
      dirReduce [North, West, South, East] `shouldBe` [North, West, South, East]
      dirReduce [North, South, South, East, West, North, West] `shouldBe` [West]
      dirReduce [North, South, South, East, West, North]       `shouldBe` []
      
    it "should return a single direction unchanged" $ do
      property $ forAll (elements [North, West, East, South]) $ \x ->
        dirReduce [x] `shouldBe` [x]
        
    it "should return a random list of north-west unchanged" $ do
      property $ forAll (listOf $ elements [North, West]) $ \xs ->
        dirReduce xs `shouldBe` xs
        
  describe "running there and back again" $ do
    forM_ [(North, South), (South, North), (West, East), (East, West)] $ \(a,b) ->
      it ("should return the right amount of " ++ show a) $ 
        property $ \t y -> t > 0 && y > 0 ==>
          let x = t + y
          in dirReduce (replicate x a ++ replicate y b) `shouldBe` replicate (x - y) a
