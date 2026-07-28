# Revision history for circuits-parser

## unreleased

- Added `Circuit.Stream.Cons` class: the left-prepend stream algebra dual to
  `Uncons`. Includes a `[a]` instance; `ByteString` and `Text` instances are not
  yet provided.
- `Circuit.Stream` now hosts the neutral stream interface: `These`, `Uncons`,
  `Cons`, and `Snoc`.
