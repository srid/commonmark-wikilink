module Commonmark.Extensions.WikiLinkSpec where

import Commonmark.Extensions.WikiLink
import Commonmark.Simple
import Test.Hspec
import Text.Pandoc.Definition

spec :: Spec
spec = do
  describe "commonmark-wikilink" $ do
    it "basic" $ do
      let res = snd <$> parseMarkdownWithFrontMatter @Text fullMarkdownSpec "<fp>" "Hello [[World]]."
          expected = Pandoc mempty [Para [Str "Hello", Space, Str "[[World]]."]]
      res `shouldBe` Right expected
    it "wikilink-parsing" $ do
      let res = snd <$> parseMarkdownWithFrontMatter @Text (fullMarkdownSpec <> wikilinkSpec) "<fp>" "Hello [[World]]."
          expected =
            Pandoc
              mempty
              [ Para
                  [ Str "Hello"
                  , Space
                  , Link ("", [], [("data-wikilink-type", "WikiLinkNormal")]) [] ("World", "")
                  , Str "."
                  ]
              ]
      res `shouldBe` Right expected
    describe "plainify" $ do
      it "basic" $ do
        plainify [Str "Hello"] `shouldBe` "Hello"
      it "with space" $ do
        plainify [Str "Hello", Space, Str "World"] `shouldBe` "Hello World"
      it "with link" $ do
        plainify [Link ("", [], []) [Str "Hello"] ("World", "")] `shouldBe` "Hello"
      it "with wikilink" $ do
        plainify [Link ("", [], [("data-wikilink-type", "WikiLinkNormal")]) [] ("World", "")] `shouldBe` "[[World]]"
