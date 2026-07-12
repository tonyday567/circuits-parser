---
name: deck
description: Card/deck dialect parser — experimental consumer of Circuit.Parser
tags: ['parser', 'deck', 'cards', 'experimental']
---
# deck ⟜ mark / elab cards on Circuit.Parser

> **Example only — not library API.** This dialect used to live as
> `Circuit.Deck`. The package surface is now **one core**
> (`Circuit.Parser*`) and **one specialty** (`Circuit.Markup*`).
> Deck belongs here as a worked consumer, same idea as other
> `~/haskell/*` sites (e.g. huihua) that depend on the core without
> shipping domain syntax from this package.

Paste into `cabal repl circuits-parser` (or a consumer that depends on
`circuits-parser`). All types and parsers are **local to this card**.

```haskell
-- $setup
-- >>> import Circuit.Parser
-- >>> import Data.These (These (..))
-- >>> import Data.Char (isAlpha, isAscii, isDigit)
-- >>> import Data.Maybe (fromMaybe)
-- >>> import Prelude hiding (many)
```

---

## mark table

| glyph | code | role |
|-------|------|------|
| ⟜ | U+27DC | mark / elab dash |
| ⟞ | U+27DD | mark (alt) |
| ⟡ | U+27E1 | symbol |
| ⇄ | U+21C4 | symbol |
| ⊲ | U+22B2 | symbol |
| ⊚ | U+29C8 | symbol |
| → | U+2192 | symbol / dash |
| … | U+2026 | symbol |
| `-` `*` | ASCII | punct dash |

---

## AST

```haskell
data Token
  = Word String
  | Symbol String
  | Mark String
  | Emoji String
  | Quoted String
  | Punct String
  deriving (Show, Eq)

newtype Dash = Dash Token deriving (Show, Eq)

data Line
  = BareLine { lead :: [Token], prose :: [Token] }
  | InlineLine { lead :: [Token], dash :: Dash, elab :: [Token] }
  | ElabLine { dash :: Dash, elab :: [Token] }
  deriving (Show, Eq)

newtype Deck = Deck { deckLines :: [Line] } deriving (Show, Eq)

newtype Card = Card { cardDecks :: [Deck] } deriving (Show, Eq)
```

---

## tokens

```haskell
wordChar :: Char -> Bool
wordChar c =
  isAscii c
    && c /= ' '
    && (isAlpha c || isDigit c || c `elem` ("-_/.@*#~" :: String))

tokenP :: Parser String Char Token
tokenP = quotedP <|> markP <|> symbolP <|> emojiP <|> punctP <|> wordP

wordP :: Parser String Char Token
wordP = Word <$> some (satisfy wordChar)

emojiP :: Parser String Char Token
emojiP =
  Emoji
    <$> some
      ( satisfy
          ( \c ->
              not (isAscii c)
                && not (c `elem` ("\x27DC\x27DD\x27E1\x21C4\x22B2\x29C8\x2192" :: String))
          )
      )

markP :: Parser String Char Token
markP = Mark <$> (string "\x27DC" <|> string "\x27DD")

symbolP :: Parser String Char Token
symbolP =
  Symbol
    <$> ( string "\x27E1"
            <|> string "\x21C4"
            <|> string "\x22B2"
            <|> string "\x29C8"
            <|> string "\x2192"
            <|> string "\x2026"
        )

quotedP :: Parser String Char Token
quotedP = Quoted <$> (char '"' *> many (satisfy (/= '"')) <* char '"')

punctP :: Parser String Char Token
punctP =
  Punct
    <$> ( string ":"
            <|> string ";"
            <|> string ","
            <|> string "("
            <|> string ")"
        )

tokensP :: Parser String Char [Token]
tokensP = tokenP >>= rest tokenP

leadTokenP :: Parser String Char Token
leadTokenP = quotedP <|> symbolP <|> emojiP <|> punctP <|> wordP

leadP :: Parser String Char [Token]
leadP = leadTokenP >>= rest leadTokenP

rest :: Parser String Char a -> a -> Parser String Char [a]
rest p first = go [first]
  where
    go acc = do
      _ <- skipWhile (== ' ')
      (p >>= \t -> go (t : acc)) <|> pure (reverse acc)
```

---

## lines / decks / cards

```haskell
dashP :: Parser String Char Dash
dashP =
  Dash
    <$> ( Mark
            <$> (string "\x27DC" <|> string "\x27DD")
              <|> Punct
            <$> (string "-" <|> string "*")
              <|> Symbol
            <$> string "\x2192"
        )

elabLineP :: Parser String Char Line
elabLineP = do
  _ <- count 2 (char ' ')
  _ <- skipWhile (== ' ')
  d <- dashP
  _ <- skipWhile (== ' ')
  ElabLine d <$> tokensP

inlineLineP :: Parser String Char Line
inlineLineP = do
  l <- leadP
  _ <- skipWhile (== ' ')
  d <- dashP
  _ <- skipWhile (== ' ')
  InlineLine l d <$> tokensP

bareLineP :: Parser String Char Line
bareLineP = do
  l <- tokensP
  p <- optional (skipWhile (== ' ') *> tokensP)
  pure $ BareLine l (fromMaybe [] p)

lineP :: Parser String Char Line
lineP = elabLineP <|> try inlineLineP <|> bareLineP

deckP :: Parser String Char Deck
deckP = lineP >>= go []
  where
    go acc l = do
      let acc' = l : acc
      next <- optional (try (char '\n' >> lineP))
      case next of
        Just l' -> go acc' l'
        Nothing -> pure (Deck (reverse acc'))

cardP :: Parser String Char Card
cardP = deckP >>= go []
  where
    go acc d = do
      let acc' = d : acc
      next <- optional (blankSep >> deckP)
      case next of
        Just d' -> go acc' d'
        Nothing -> pure (Card (reverse acc'))
    blankSep = char '\n' >> char '\n' >> skipWhile (== '\n')

parseCard :: String -> Either String Card
parseCard input =
  case runParser (cardP <* many (char '\n') <* endOfInput) input of
    This c -> Right c
    These c _ -> Right c
    That s -> Left ("parse failed at: " ++ take 60 s)
```

---

## smoke

```haskell
plain :: String
plain =
  "markdown \x27E1 general\n\
  \  \x27DC words know themselves by the company they keep\n\
  \  \x27DC collusion without collision.\n"

-- >>> case parseCard plain of Right (Card ds) -> length ds; _ -> -1
-- 1

-- >>> case parseCard plain of Right c -> length (deckLines (head (cardDecks c))); _ -> -1
-- 3
```

Choice order matters: `elabLineP` (indent + dash) before `try inlineLineP`
before `bareLineP`. `try` restores the stream when inline fails after
consuming a lead.

---

## package map

| import | status |
|--------|--------|
| `Circuit.Parser` (+ Lexer, Token, Primitives) | **core** — stream coalgebra + combinators |
| `Circuit.Markup` | **specialty** — HTML/XML-ish pipeline |
| this card | **example** — card/deck dialect; promote in a consumer if needed |

External consumers (huihua, chart-svg-dev, …) should depend on **core**
(and markup if needed), not on a deck module from this package.
