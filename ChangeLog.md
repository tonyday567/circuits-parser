# Revision history for circuits-parser

## unreleased

- Added `span`, `span1`, and `peek` to `Circuit.Parser`.
- `sepBy1` now commits: a trailing separator is a parse failure rather than
  being silently swallowed. Use `try` around the separator if you genuinely
  want the old trailing-separator behaviour.
- `Circuit.Parser` module haddock now states the intact-stream law and the
  role of `try`.
- `Circuit.Stream` now warns that the `Uncons ByteString Char` instance
  exposes raw bytes as `Char`s, not decoded UTF-8.
- Readme error-path signpost: combinator failures are flat; positioned errors
  belong in the lexer layer or a different base monad.
- Added `Circuit.Parser.Json*` and `Circuit.Parser.Csv*` dialects.
