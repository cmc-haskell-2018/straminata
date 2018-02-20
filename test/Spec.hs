-- file Spec.hs
import Test.Hspec

import Model.CommonTypes
import Util.Common

main :: IO ()
main = hspec $ do
  let h1 = (Position (0, 0), Position (2, 2))
      h2 = (Position (1, 1), Position (3, 3))
      h3 = (Position (3, 0), Position (5, 2))
      h4 = (Position (-1, -1), Position (1, 1))
  describe "Prelude.head" $ do
    it "checks if rectangles collide" $ do
      collide h1 h2 `shouldBe` True
    it "checks if rectangles collide" $ do
      collide h1 h3 `shouldBe` False
    it "checks if rectangles collide" $ do
      collide h1 h4 `shouldBe` True
