-- file Spec.hs
import Test.Hspec

import Model.CommonTypes

main :: IO ()
main = hspec $ do
  let h1 = Rectangle (Position (0, 0), Position (2, 2))
      h2 = Rectangle (Position (1, 1), Position (3, 3))
      h3 = Rectangle (Position (3, 0), Position (5, 2))
      h4 = Rectangle (Position (-1, -1), Position (1, 1))
      p1 = Position (1, 1)
  describe "Prelude.head" $ do
    it "checks if rectangles collide" $ do
      collide h1 h2 `shouldBe` True
    it "checks if rectangles collide" $ do
      collide h1 h3 `shouldBe` False
    it "checks if rectangles collide" $ do
      collide h1 h4 `shouldBe` True
