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
    it "decodes HTML entities in custom titles" $ do
      parseMdPara1 "[[category-theory-spivak-2014|Spivak&nbsp;(2014)]]"
        `shouldBe` Right
          [ Link
              ("", [], [("data-wikilink-type", "WikiLinkNormal")])
              [Str "Spivak\160(2014)"]
              ("category-theory-spivak-2014", "")
          ]
    it "decodes numeric HTML entities in custom titles" $ do
      parseMdPara1 "[[paper|Section &#35;1]]"
        `shouldBe` Right
          [ Link
              ("", [], [("data-wikilink-type", "WikiLinkNormal")])
              [Str "Section", Space, Str "#1"]
              ("paper", "")
          ]
    it "keeps numeric HTML entities in references distinct from anchors" $ do
      parseMdPara1 "[[chapter-&#35;1|number]]"
        `shouldBe` Right
          [ Link
              ("", [], [("data-wikilink-type", "WikiLinkNormal")])
              [Str "number"]
              ("chapter-%231", "")
          ]
    describe "anchors in wikilinks" $ do
      it "preserves anchor in cross-file heading link" $ do
        parseMdPara1 "[[note#heading]]"
          `shouldBe` Right
            [ Link
                ("", [], [("data-wikilink-type", "WikiLinkNormal")])
                []
                ("note#heading", "")
            ]
      it "preserves anchor with custom title" $ do
        parseMdPara1 "[[note#heading|See heading]]"
          `shouldBe` Right
            [ Link
                ("", [], [("data-wikilink-type", "WikiLinkNormal")])
                [Str "See", Space, Str "heading"]
                ("note#heading", "")
            ]
      it "preserves anchor in embed link" $ do
        parseMdPara1 "![[note#heading]]"
          `shouldBe` Right
            [ Link
                ("", [], [("data-wikilink-type", "WikiLinkEmbed")])
                []
                ("note#heading", "")
            ]
      it "preserves anchor in branch link" $ do
        parseMdPara1 "[[note#heading]]#"
          `shouldBe` Right
            [ Link
                ("", [], [("data-wikilink-type", "WikiLinkBranch")])
                []
                ("note#heading", "")
            ]
      it "preserves anchor in tag link" $ do
        parseMdPara1 "#[[note#heading]]"
          `shouldBe` Right
            [ Link
                ("", [], [("data-wikilink-type", "WikiLinkTag")])
                []
                ("note#heading", "")
            ]
      it "parses anchor with spaces" $ do
        parseMdPara1 "[[note#hello world]]"
          `shouldBe` Right
            [ Link
                ("", [], [("data-wikilink-type", "WikiLinkNormal")])
                []
                ("note#hello world", "")
            ]
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
      it "with wikilink anchor" $ do
        plainify <$> parseMdPara1 "[[note#heading]]" `shouldBe` Right "[[note#heading]]"
      it "with wikilink anchor and custom title yields the custom text" $ do
        plainify <$> parseMdPara1 "[[note#heading|See heading]]" `shouldBe` Right "See heading"
      it "with footnote" $ do
        plainify <$> parseMdPara1 "Hello[^1] World.\n\n[^1]: Some footnote." `shouldBe` Right "Hello World."
      it "with quotes" $ do
        plainify <$> parseMdPara1 "Foo \"Bar\" - MySite" `shouldBe` Right "Foo “Bar” - MySite"
      it "with emoji" $ do
        plainify <$> parseMdPara1 "Emoji :writing_hand:" `shouldBe` Right "Emoji ✍️"
      it "erases strikethroughs" $ do
        plainify <$> parseMdPara1 "Hello ~~wonderful~~ world!" `shouldBe` Right "Hello world!"

-- | Parse Markdown with our wikilink parser enabled
parseMd :: Text -> Either Text Pandoc
parseMd = fmap snd . parseMarkdownWithFrontMatter @Text (fullMarkdownSpec <> wikilinkSpec) "<fp>"

-- | Like `parseMd` but get just the first paragraph
parseMdPara1 :: Text -> Either Text [Inline]
parseMdPara1 s = do
  Pandoc _ (Para inlines : _) <- parseMd s
  pure inlines
