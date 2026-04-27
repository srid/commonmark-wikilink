
## Unreleased

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
