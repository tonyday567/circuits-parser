# Revision history for circuits-parser

## unreleased

- Added `Circuit.Parser.Framing`: strict JSON/JSONL framing for stamped text
  posts (`Stored`, `encodeStored`/`decodeStored`, `encodeRow`/`decodeRow`,
  `encodeLog`/`decodeLog`). Logs stay oldest-first and a malformed row is
  rejected with its row number, offset and reason rather than dropped.
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
- Renamed `Circuit.Stream` to `Circuit.Parser.Stream` to avoid the module-name
  clash with `circuits` and eliminate `PackageImports`.
