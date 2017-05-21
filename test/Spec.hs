{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StandaloneDeriving #-}
import Data.Text.Format.Simple
import Test.Hspec
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Lens.Micro.Platform


deriving instance Show Compiled
deriving instance Eq Compiled
deriving instance Eq Part
deriving instance Show Name
deriving instance Show Part

parserSpec :: Spec
parserSpec = describe "the parser" $ do
    it "parses a string to a string" $
        parse "a literal string" `shouldBe` Right [String "a literal string"]
    it "parses a variable" $
        parse "${var}" `shouldBe` Right [NamedVar $ Name "var"]
    it "parses a string followed by a variable" $
        parse "str ${var}" `shouldBe` Right [String "str ", NamedVar $ Name "var"]
    it "parses a string followed by a variable w/o whitespace" $
        parse "str${var}" `shouldBe` Right [String "str", NamedVar $ Name "var"]
    it "parses a string followed by a variable followed by a string" $
        parse "str ${var} str2" `shouldBe` Right [String "str ", NamedVar $ Name "var", String " str2"]
    it "parses a literal '$'" $
        parse "$" `shouldBe` Right [String "$"]
    it "parses several consecutive literal '$'" $
        parse "$$$$" `shouldBe` Right [String "$$$$"]


sp :: Compiled -> HashMap Name Text -> (Text, [Name])
sp = substitutePrecompiled


substitutionSpec :: Spec
substitutionSpec =
    describe "substitutePrecompiled" $ do
        it "substitutes a single named variable to itself" $
            sp "${var}" [("var", "value")] `shouldBe` ("value", [])
        it "substitutes a variable between text" $
            sp "do ${var} right" [("var", "it")] `shouldBe` ("do it right", [])
        it "substitutes several variables" $
            sp "counting from ${one} to ${two}" [("one", "twenty"), ("two", "fourty")] `shouldBe` ("counting from twenty to fourty", [])
        it "reports missing names" $
            sp "counting from ${one} to ${two}" [("one", "twenty")] `shouldBe` ("counting from twenty to ", ["two"])


main :: IO ()
main = hspec $ do
    parserSpec
    substitutionSpec
