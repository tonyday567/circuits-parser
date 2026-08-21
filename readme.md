<p align="center"><strong>⟴ circuits-parser</strong></p>

Parser combinators for the circuits ecosystem. A `Parser` is a circuit whose base arrow decomposes a stream one element at a time. Both the primitive that peels a stream (`uncons`) and the composite that runs a parse (`runParser`) are `These`-coalgebras on the stream type `f`. That single sentence is the whole design.

The stream coalgebra (`These`, `Uncons`) lives in `Circuit.Stream`; `Circuit.Parser` re-exports it and layers the traced-monoidal parser syntax on top.

## the shape

Start from what it means to take a coinductive thing apart one element at a time. One peel yields *at least one of* {head, tail} — never neither, since neither is no observation at all. So the codomain is an inclusive-or-with-witness, and that is exactly `These`:

```haskell
data These a b = This a | That b | These a b
```

For a stream `f` of elements `s`, one peel is a `These s`-step over `f`:

```haskell
uncons :: f -> These s f
```

Read the right argument as the carrier and partially apply the bifunctor on the left: `These s f` is `(These s) f`. Then `G = These s` is an endofunctor, and

```haskell
uncons :: f -> G f
```

is a **coalgebra for `G`**. Nothing more, nothing less. The class `Uncons f s` is the statement "`f` carries a `These s`-coalgebra." The final coalgebra `ν(These s)` is the free stream-of-`s`-that-knows-how-it-ends; `uncons` is its structure map, and finality hands you the anamorphism `f -> ν(These s)` for free. That is the canonical recipe for anything that decomposes one element at a time — pick the base functor, and a coalgebra for it *is* the coinductive stream.

The three constructors are forced by the functor, not chosen:

| constructor | meaning |
|-------------|---------|
| `That f`      | nil — no element, carrier intact (no progress) |
| `These s f`   | a `cons` — element `s`, carrier shrinks to `f` |
| `This s`      | the final element — carrier now spent |

## why not `Maybe (s, f)`

`Maybe (s, f)` is the coalgebra for a *different* functor, `H x = 1 + s × x` — the inductive `ListF`. Same objects, wrong arrows. `H` cannot emit-and-terminate in a single step, so:

- `This` is unnameable. Head-without-tail has nowhere to live.
- End arrives a peel late. You consume `(x, [])`, peek again, and *then* get `Nothing`. Termination is a separate nullary observation reached *after* the last element.

`These` fuses termination into the productive step: `This x` announces "this was the last one" *at* extraction — one observation, maximally informative per peel. That is what "defined by how it is deconstructed" buys. `Maybe` is the inductive recipe (μ, base functor `ListF`); `These` is the coinductive recipe (ν). You would never build a stream on `1 + s × X`, and you should not build `uncons` on it either.

The corollary is a rule for instances: **inhabit `This`.** A nil-check that returns `That` for the last element and leans on a separate emptiness guard is the inductive base case smuggled back in — induction against a coinductive carrier. Category error, not a style nit.

The final element is announced at extraction rather than a peel late:

```haskell
instance Uncons [a] a where
  uncons []       = That []      -- nil
  uncons [x]      = This x       -- final element, announced at extraction
  uncons (x : xs) = These x xs   -- cons
```

For strict, measured streams (`ByteString`, `Text`) the end is known in O(1), so `This` is free and honest — each instance reuses the stream's own `uncons` plus an O(1) `null` on the remainder (never a length measurement; `Text` length is O(n)). For a lazy `[a]`, distinguishing `[x]` from `x : xs` forces one cell of look-ahead — laziness presenting its bill exactly where the stream is lazy. An **infinite** list therefore never fires `This`, which is correct: `ν(These s)` declines to announce an end that does not exist.

`This` originates at the coalgebra: the atoms (`satisfy`, `anyToken`, `satisfyAscii`) give it a home by matching the final element as `This s`. `pure` does not inspect the stream; an exact match therefore leaves an empty remainder as `These a mempty` rather than forcing a final `This` inside the parser. Boundary extractors (`runParserMaybe`, `runParserError`, and the markup runners) can collapse an empty remainder to `This` if they choose, but `runParser` itself stays raw. `endOfInput` remains just "`uncons` returned `That`" — no `HasLength`, and it works for `Text`, which carries no length instance.

## the parser is the same shape

```haskell
newtype Parser f s a = Parser { unParser :: Trace Either (->) f (These a f) }

runParser :: Parser f s a -> f -> These a f
```

`runParser p` has type `f -> These a f` — a `These a`-coalgebra step on `f`, the very same shape as `uncons`. This is not a coincidence dressed up as a theorem, nor a pun. Both are single-step decompositions of a coinductive process, so both land on `These`-over-`f` for the same reason, independently. Common cause, not one deriving the other.

The base category of `Parser` is exactly the arrows `f -> These · f` — the coalgebra shape. `uncons` lives in that base category: `anyToken` is morally `Arr uncons`. The primitive decomposition and the composite parse are the **same species of arrow**, which is precisely what lets you build parsers by composing peels. A `Parser` is the free trace over `These`-coalgebra steps on `f`.

## two tensors, two jobs

`Parser f s a = Trace Either (->) f (These a f)` carries both tensors of the circuits core, each doing a distinct job:

| tensor | axis | role in the parser |
|--------|------|--------------------|
| `These` | value / data | produced-element ⊗ remainder — the coinductive decomposition |
| `Either` | control / feedback | `Left` feeds the intact stream back, `Right` commits — backtracking |

So a parser is coalgebraic decomposition (`These`) threaded through a traced-monoidal control structure (`Either`). `uncons` is the atom of the first axis; the `Knot` inside `<|>` is the atom of the second — `Left s'` feeds the untouched stream back to the alternative, `Right` commits to a result.

## running

`runParser` returns the raw `These`; the constructor tells you what happened. (All examples are verified doctest output.)

```haskell
runParser (char 'a') "abc"      -- These 'a' "bc"    consumed, more left
runParser (char 'x') "abc"      -- That "abc"        no progress, stream intact
runParser (string "abc") "abc"  -- These "abc" ""    consumed to the end
runParser (string "abc") "abcd" -- These "abc" "d"   consumed, "d" left
runParser endOfInput ""         -- This ()           uncons returned That
```

`string` does not special-case the final step; it is the `Uncons` instance that announces `This` when the last element is consumed. `pure` does not inspect the stream, so `runParser (many (char 'a')) "aaa"` is `These "aaa" ""` from the raw runner. Boundary extractors can collapse an empty remainder to `This` where finiteness belongs.

- `This a`    — consumed everything, `a` is the final result (only from `Uncons` or an extractor).
- `That f`    — no progress, stream returned intact (this is what `<|>` backtracks on).
- `These a f` — consumed some, `a` with remainder `f`; an empty `f` means the input was exactly exhausted.

Extractors specialise the raw result at the point of consumption:

```haskell
runParserMaybe :: (Uncons f s) => Parser f s a -> f -> Maybe a
runParserError :: (Uncons f s) => Parser f s a -> f -> a
```

### errors

The combinator runner has no position channel — a failure is just 'That'
with the intact stream, so boundary extractors report a flat failure. For
positioned or structured errors, change the base monad: `StateT s
(ExceptT e n)` carries offsets, `LogicT n` supports nondeterminism. The
lexer layer (`Circuit.Parser.Lexer`, `Circuit.Parser.Json.Lexer`,
`Circuit.Parser.Csv.Lexer`) keeps byte offsets and is the right place to
report "unexpected X at offset Y".

## package map

| layer | modules | role |
|-------|---------|------|
| **core** | `Circuit.Parser`, `.Lexer`, `.Token`, `.Primitives` | stream coalgebra + combinators + fast lexers |
| **dialects** | `Circuit.Parser.Json*`, `Circuit.Parser.Csv*` | canonical domain parsers on the core — the worked examples of the coalgebra claim |
| **specialty** | `Data.Markup*` (chart-svg) | HTML/XML-ish tokenize → gather → render |
| **examples** | `examples/deck.md` | domain dialects (card/deck marks) — not library API |

External consumers (huihua, chart-svg-dev, …) depend on **core** (and
`Data.Markup` from chart-svg when needed). They own their dialects; this
package does not ship them.

### core

`Circuit.Parser.Primitives` — numeric parsers (`digit`, `int`, `double`, `signed`) and UTF-8 conversions.

`Circuit.Parser.Token` — word/number/punctuation tokenisers and vocabulary building over `[Text]`.

`Circuit.Parser.Lexer` — fast imperative `ByteString` lexers (zero-copy slices), complementary to the combinator path: they trade compositionality for raw speed, and the markup lexer doubles as a state machine that morphs into a `Circuit` when composition is needed.

### specialty

`Data.Markup` (chart-svg) — HTML/XML-ish markup pipeline on the combinators
above. Formerly `Circuit.Markup` in this package.

### examples

`examples/deck.md` — mark/elab **card** dialect (former `Circuit.Deck`), paste-into-repl card.
