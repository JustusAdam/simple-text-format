{-# LANGUAGE OverloadedLists    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
import           Data.HashMap.Strict              (HashMap)
import           Data.Text                        (Text)
import           Data.Text.Format.Simple
import           Data.Text.Format.Simple.Internal
import           Lens.Micro.Platform
import           Test.Hspec
import GHC.Exts


instance IsList Compiled where
    type Item Compiled = Part
    fromList = Compiled
    toList = unCompiled

parserSpec :: Spec
parserSpec = describe "the parser" $ do
    it "parses a string to a string" $
        parse "a literal string" `shouldBe` Right [String "a literal string"]
    it "parses a variable" $
        parse "${var}" `shouldBe` Right [NamedVar "var"]
    it "parses a string followed by a variable" $
        parse "str ${var}" `shouldBe` Right [String "str ", NamedVar "var"]
    it "parses a string followed by a variable w/o whitespace" $
        parse "str${var}" `shouldBe` Right [String "str", NamedVar "var"]
    it "parses a string followed by a variable followed by a string" $
        parse "str ${var} str2" `shouldBe` Right [String "str ", NamedVar "var", String " str2"]
    it "parses a literal '$'" $
        parse "$" `shouldBe` Right [String "$"]
    it "parses several consecutive literal '$'" $
        parse "$$$$" `shouldBe` Right [String "$$$$"]


formatSpec :: Spec
formatSpec = do
    let subMap :: HashMap Name Text
        subMap = [("one", "twenty"), ("two", "fourty"), ("var", "value"), ("var2", "it")]
        lookup k = subMap ^. at k
        sp :: Compiled -> (Text, [Name])
        sp = flip formatC lookup

    describe "formatC" $ do
        it "substitutes a single named variable to itself" $
            sp "${var}" `shouldBe` ("value", [])
        it "substitutes a variable between text" $
            sp "do ${var2} right" `shouldBe` ("do it right", [])
        it "substitutes several variables" $
            sp "counting from ${one} to ${two}" `shouldBe` ("counting from twenty to fourty", [])
        it "reports missing names" $
            sp "counting from ${one} to ${three}" `shouldBe` ("counting from twenty to ", ["three"])


main :: IO ()
main = hspec $ do
    parserSpec
    formatSpec
