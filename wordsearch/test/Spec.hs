import Test.Hspec
import Lib
import Data

gwc = gridWithCoords grid

cellsToString :: Maybe [Cell] -> Maybe String
cellsToString cells =
  case cells of
    Just cs -> Just $ map cell2char cs
    Nothing -> Nothing

main :: IO ()
main = hspec $ do
  describe "formatGrid" $ do
    it "Should concatenate every line with a newline" $ do
      (formatGrid $ gridWithCoords ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"

  describe "findWord" $ do
    it "Should find the word in grid" $ do
      (cellsToString $ findWord gwc "RUBY") `shouldBe` Just "RUBY"
      (cellsToString $ findWord gwc "HASKELL") `shouldBe` Just "HASKELL"

    it "Shouldn't find the word and return Nothing" $ do
      (cellsToString $ findWord gwc "ERLANG") `shouldBe` Nothing

  {-describe "findWords" $ do-}
    {-it "Should find the words in grid" $ do-}
      {-(findWords gwc languages) `shouldBe` languages-}

    {-it "Shouldn't find the words in grid" $ do-}
      {-(findWords gwc ["SQL", "COUCHDB"]) `shouldBe` []-}
