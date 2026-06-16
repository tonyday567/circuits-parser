{-# LANGUAGE OverloadedStrings #-}

-- | Deck parser built on 'Circuit.Parser'.
module Circuit.Deck
  ( Token (..),
    Dash (..),
    Line (..),
    Deck (..),
    Card (..),
    tokenP,
    tokensP,
    leadP,
    dashP,
    elabLineP,
    inlineLineP,
    bareLineP,
    lineP,
    deckP,
    cardP,
    parseCard,
    parseCard_,
  )
where

import Circuit.Parser
  ( Parser,
    asThese,
    char,
    count,
    endOfInput,
    many,
    runParser,
    satisfy,
    skipWhile,
    some,
    string,
    try,
    (<|>),
  )
import Data.Char (isAlpha, isAscii, isDigit)
import Data.These (These (..))

-- AST

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
  = BareLine {lead :: [Token], prose :: [Token]}
  | InlineLine {lead :: [Token], dash :: Dash, elab :: [Token]}
  | ElabLine {dash :: Dash, elab :: [Token]}
  deriving (Show, Eq)

newtype Deck = Deck {deckLines :: [Line]} deriving (Show, Eq)

newtype Card = Card {cardDecks :: [Deck]} deriving (Show, Eq)

-- Token parsers

wordChar :: Char -> Bool
wordChar c = isAscii c && c /= ' ' && (isAlpha c || isDigit c || c `elem` ("-_/.@*#~" :: String))

tokenP :: Parser String Char Token
tokenP = quotedP <|> markP <|> symbolP <|> emojiP <|> punctP <|> wordP

wordP :: Parser String Char Token
wordP = Word <$> some (satisfy wordChar)

emojiP :: Parser String Char Token
emojiP = Emoji <$> some (satisfy (\c -> not (isAscii c) && not (isMathSym c)))
  where
    isMathSym c = c `elem` ("\x27DC\x27DD\x27E1\x21C4\x22B2\x29C8\x2192" :: String)

markP = Mark <$> (string "\x27DC" <|> string "\x27DD")

symbolP =
  Symbol
    <$> ( string "\x27E1"
            <|> string "\x21C4"
            <|> string "\x22B2"
            <|> string "\x29C8"
            <|> string "\x2192"
            <|> string "\x2026"
        )

quotedP = Quoted <$> (char '"' *> many (satisfy (/= '"')) <* char '"')

punctP = Punct <$> (string ":" <|> string ";" <|> string "," <|> string "(" <|> string ")")

-- Token sequence: first token, then zero or more (space+ token).
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

-- Dash

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

-- Line parsers

elabLineP :: Parser String Char Line
elabLineP = do
  _ <- count 2 (char ' ')
  _ <- skipWhile (== ' ')
  d <- dashP
  _ <- skipWhile (== ' ')
  e <- tokensP
  pure $ ElabLine d e

inlineLineP :: Parser String Char Line
inlineLineP = do
  l <- leadP
  _ <- skipWhile (== ' ')
  d <- dashP
  _ <- skipWhile (== ' ')
  e <- tokensP
  pure $ InlineLine l d e

bareLineP :: Parser String Char Line
bareLineP = do
  l <- tokensP
  p <- opt (skipWhile (== ' ') *> tokensP)
  pure $ BareLine l (maybe [] id p)
  where
    opt q = (Just <$> q) <|> pure Nothing

lineP :: Parser String Char Line
lineP = elabLineP <|> try inlineLineP <|> bareLineP

-- Deck: one or more lines separated by newlines.
-- Stops at blank lines (two consecutive newlines) to let cardP handle deck separation.
deckP :: Parser String Char Deck
deckP = lineP >>= go []
  where
    go acc l = do
      let acc' = l : acc
      next <- tryNext (try (char '\n' >> lineP))
      case next of
        Just l' -> go acc' l'
        Nothing -> pure (Deck (reverse acc'))
    tryNext p = (Just <$> p) <|> pure Nothing

-- Card: one or more decks separated by blank lines
cardP :: Parser String Char Card
cardP = deckP >>= go []
  where
    go acc d = do
      let acc' = d : acc
      next <- tryNext (blankSep >> deckP)
      case next of
        Just d' -> go acc' d'
        Nothing -> pure (Card (reverse acc'))
    blankSep = char '\n' >> char '\n' >> skipWhile (== '\n')
    tryNext p = (Just <$> p) <|> pure Nothing

-- Running

parseCard :: String -> Either String Card
parseCard input = case runParser (cardP <* many (char '\n') <* endOfInput) input of
  This c -> Right c
  These c _ -> Right c
  That s -> Left ("parse failed at: " ++ take 60 s)

parseCard_ :: String -> Card
parseCard_ = asThese . runParser cardP
