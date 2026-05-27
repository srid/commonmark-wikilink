
## Unreleased

- Preserve `#anchor` in wikilinks so `[[note#heading]]` renders as `note#heading` instead of dropping the fragment ([srid/emanote#105](https://github.com/srid/emanote/discussions/105)).
    - **Breaking**: `wikilinkInline` now takes `Maybe Anchor`; `mkWikiLinkFromInline` returns `(WikiLink, Maybe Anchor, [Inline])`.
- Decode HTML entities in wikilink custom titles while preserving entity escapes in references as link text, not anchor delimiters ([#9](https://github.com/srid/commonmark-wikilink/pull/9)).

## 0.2.0.0

- Fixes
    - `plainify`
        - Fix double scanning of inlines of wiki-links
        - Remove footnotes (\#3)
        - Fix `Quoted` inline processing
        - Fix dealing with strikeouts by erasing them (\#6)

## 0.1.0.0

- Initial release
