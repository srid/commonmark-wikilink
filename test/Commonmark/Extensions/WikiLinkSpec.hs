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
        plainify <$> parseMdPara1 "Hello" `shouldBe` Right "Hello"
      it "with space" $ do
        plainify <$> parseMdPara1 "Hello World" `shouldBe` Right "Hello World"
      -- FIXME
      xit "with link" $ do
        plainify <$> parseMdPara1 "[Hello](https://example.com)" `shouldBe` Right "Hello"
      it "with wikilink" $ do
        plainify <$> parseMdPara1 "[[World]]" `shouldBe` Right "[[World]]"
      -- FIXME
      xit "with footnote" $ do
        plainify <$> parseMdPara1 "Hello[^1] World.\n\n[^1]: Some footnote." `shouldBe` Right "Hello World."

-- | Parse Markdown with our wikilink parser enabled
parseMd :: Text -> Either Text Pandoc
parseMd = fmap snd . parseMarkdownWithFrontMatter @Text (fullMarkdownSpec <> wikilinkSpec) "<fp>"

-- | Like `parseMd` but get just the first paragraph
parseMdPara1 :: Text -> Either Text [Inline]
parseMdPara1 s = do
  Pandoc _ (Para inlines : _) <- parseMd s
  pure inlines
