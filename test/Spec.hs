import AdventFiveSolutions
import Control.Exception (evaluate)
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "Advent of Code 2015" $ do
      describe "Utils" $ do
        describe "Stepped list" $ do
          it "step every two" $ do
            stepEvery 2 [1..9] `shouldBe` [1, 3 .. 9]
          it "step every four" $ do
            stepEvery 4 [0..8] `shouldBe` [0, 4, 8]
      describe "Day 1 - Not Quite Lisp - Part 1" $ do
        describe "Paren examples" $ do
          it "(())" $ do
            notQuiteLisp "(())" `shouldBe` 0
          it "()()" $ do
            notQuiteLisp "()()" `shouldBe` 0
          it "(((" $ do
            notQuiteLisp "(((" `shouldBe` 3
          it "(()(()(" $ do
            notQuiteLisp "(()(()(" `shouldBe` 3
          it "(()(()(" $ do
            notQuiteLisp "))(((((" `shouldBe` 3
          it "())" $ do
            notQuiteLisp "())" `shouldBe` -1
          it "))(" $ do
            notQuiteLisp "))(" `shouldBe` -1
          it ")))" $ do
            notQuiteLisp ")))" `shouldBe` -3
          it ")())())" $ do
            notQuiteLisp ")())())" `shouldBe` -3
        describe "Day 1 - Not Quite Lisp - Part 2" $ do
          it "(())" $ do
            notQuiteLispPartTwo ")" `shouldBe` 1
          it "()()" $ do
            notQuiteLispPartTwo "()())" `shouldBe` 5
      describe "Day 2 - I Was Told There Would Be No Math - Part 1" $ do
        describe "Single surface" $ do
          it "2x3x4" $ do
            paperSurface 2 3 4 `shouldBe` 58
          it "1x1x10" $ do
            paperSurface 1 1 10 `shouldBe` 43
        describe "Single surface with struct argument" $ do
          it "2x3x4" $ do
            paperSurfaceDim (Dimensions 2 3 4) `shouldBe` 58
          it "1x1x10" $ do
            paperSurfaceDim (Dimensions 1 1 10) `shouldBe` 43
        describe "Line parser" $ do
          it "2x3x4" $ do
            parseDimLine "2x3x4" `shouldBe` Dimensions 2 3 4
          it "1x1x10" $ do
            parseDimLine "1x1x10" `shouldBe` Dimensions 1 1 10
        describe "Problem input" $ do
          it "2x3x4\\n1x1x10" $ do
            sumPaperSurface "2x3x4\n1x1x10" `shouldBe` 58 + 43
      describe "Day 2 - I Was Told There Would Be No Math - Part 2" $ do
        describe "Ribbon calculator" $ do
          it "2x3x4" $ do
            ribbon (Dimensions 2 3 4) `shouldBe` 34
          it "1x1x10" $ do
            ribbon (Dimensions 1 1 10) `shouldBe` 14
        describe "Ribbon calculator from string" $ do
          it "2x3x4" $ do
            sumRibbon "2x3x4" `shouldBe` 34
          it "1x1x10" $ do
            sumRibbon "1x1x10" `shouldBe` 14
      describe "Day 3 - Perfectly Spherical Houses in a Vacuum - Part 1" $ do
        describe "Commit movement" $ do
          it "Move to North" $ do
            move (0, 7) '^' `shouldBe` (0, 8)
          it "Move to South" $ do
            move (2, 5) 'v' `shouldBe` (2, 4)
          it "Move to East" $ do
            move (2, 2) '>' `shouldBe` (3, 2)
          it "Move to South" $ do
            move (2, 5) '<' `shouldBe` (1, 5)
        describe "Coordinate set" $ do
          it ">" $ do
            uniqueCoordinates ">" `shouldBe` 2
          it "^>v<" $ do
            uniqueCoordinates "^>v<" `shouldBe` 4
          it "^v^v^v^v^v" $ do
            uniqueCoordinates "^v^v^v^v^v" `shouldBe` 2
        describe "Alternating coordinate set" $ do
          it "^v" $ do
            alternatingUniqueCoordinates "^v" `shouldBe` 3
          it "^>v<" $ do
            alternatingUniqueCoordinates "^>v<" `shouldBe` 3
          it "^v^v^v^v^v" $ do
            alternatingUniqueCoordinates "^v^v^v^v^v" `shouldBe` 11


