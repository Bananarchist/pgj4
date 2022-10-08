module Main (main) where

import Test.Hspec
import qualified Unit.LogicSpec


main :: IO ()
main = hspec $ do
 describe "Logic" Unit.LogicSpec.spec
