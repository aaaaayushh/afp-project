module CustomMapTests where

import CustomMap
import Lang.Abs (Ident (..))
import Test.Hspec

-- Test data
key1 :: Ident
key1 = Ident "a"

val1 :: Integer
val1 = 1

key2 :: Ident
key2 = Ident "b"

val2 :: Integer
val2 = 2

key3 :: Ident
key3 = Ident "c"

val3 :: Integer
val3 = 3

test :: IO ()
test = hspec $ do
  describe "CustomMap" $ do
    it "starts empty" $ do
      let m = empty :: CustomMap Ident Integer
      CustomMap.lookup key1 m `shouldBe` Nothing

    it "inserts a new key-value pair" $ do
      let m = insert key1 val1 empty
      CustomMap.lookup key1 m `shouldBe` Just val1

    it "updates an existing key" $ do
      let m1 = insert key1 val1 empty
      let m2 = insert key1 val2 m1
      CustomMap.lookup key1 m2 `shouldBe` Just val2

    it "handles multiple insertions" $ do
      let m = insert key1 val1 . insert key2 val2 . insert key3 val3 $ empty
      CustomMap.lookup key1 m `shouldBe` Just val1
      CustomMap.lookup key2 m `shouldBe` Just val2
      CustomMap.lookup key3 m `shouldBe` Just val3

    it "returns Nothing for a key that does not exist" $ do
      let m = insert key1 val1 . insert key2 val2 $ empty
      CustomMap.lookup key3 m `shouldBe` Nothing