{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parsing functions for markup.
module Circuit.Markup.Parse
  ( markup,
    markup_,
    tokenize,
    tokenize_,
    tokenP,
    nameP,
    attrsP,
    runMarkupParser,
    runParser_,
    runParserWarn,
    ws,
    ws_,
    doctypeHtml,
    doctypeXml,
    -- * Internal parsers
    tokenHtmlP,
    tokenXmlP,
    isWhitespace,
    isNameChar,
    isNameCharXml,
    isNameStartChar,
    isAttrName,
    isBooleanAttrName,
    bs,
    eq_,
    wrappedQ,
    nameStartCharXmlP,
    nameCharXmlP,
    nameXmlP,
    commentP_,
    contentP_,
    declXmlP_,
    doctypeXmlP_,
    startTagsXmlP_,
    attrXmlP_,
    endTagXmlP_,
    nameHtmlP,
    startTagsHtmlP_,
    endTagHtmlP_,
    attrHtmlP_,
    attrsHtmlP_,
    doctypeHtmlP_,
    bogusCommentHtmlP_,
  )
where

import Circuit.Markup.Tree (gather)
import Circuit.Markup.Types
import Circuit.Markup.Warn (warnError)
import Circuit.Parser
  ( Parser,
    captured,
    char,
    many,
    satisfy,
    skipWhile,
    some,
    string,
    (<|>),
  )
import Circuit.Parser qualified as CP
import Circuit.Parser.Primitives (isLatinLetter)
import Control.Monad
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Char
import Data.Function
import Data.These

-- $setup
-- >>> :set -XOverloadedStrings

-- | Convert bytestrings to 'Markup'
--
-- Two-phase pipeline: lexical (tokenize) then semantic (gather)
--
-- >>> markup Html "<foo><br></foo><baz"
-- These [MarkupParser (ParserLeftover "<baz")] (Markup {elements = [Node {rootLabel = OpenTag StartTag "foo" [], subForest = [Node {rootLabel = OpenTag StartTag "br" [], subForest = []}]}]})
markup :: Standard -> ByteString -> Warn Markup
markup s b = b & (tokenize s >=> gatherTokens s)

-- | 'markup' but errors on warnings.
markup_ :: Standard -> ByteString -> Markup
markup_ s b = markup s b & warnError

-- | Wrapper for gather to work with Kleisli composition in markup pipeline
gatherTokens :: Standard -> [Token] -> Warn Markup
gatherTokens s ts = case runTP (gather s) ts of
  ([], result) -> result
  _ -> error "Impossible: gather should consume all tokens"

-- | A 'Token' parser.
--
-- >>> runMarkupParser (tokenP Html) "<foo>content</foo>"
-- ("content</foo>",These () (OpenTag StartTag "foo" []))
tokenP :: Standard -> Parser String Char Token
tokenP Html = tokenHtmlP
tokenP Xml = tokenXmlP

-- | Parse a bytestring into tokens
--
-- >>> tokenize Html "<foo>content</foo>"
-- That [OpenTag StartTag "foo" [],Content "content",EndTag "foo"]
tokenize :: Standard -> ByteString -> Warn [Token]
tokenize s b = first ((: []) . MarkupParser) $ runParserWarn (many (tokenP s)) (B.unpack b)

-- | tokenize but errors on warnings.
tokenize_ :: Standard -> ByteString -> [Token]
tokenize_ s b = tokenize s b & warnError

-- | Standard Html Doctype
doctypeHtml :: Markup
doctypeHtml = Markup $ pure $ pure (Doctype "DOCTYPE html")

-- | Standard Xml Doctype
doctypeXml :: Markup
doctypeXml =
  Markup
    [ pure $ Decl "xml" [Attr "version" "1.0", Attr "encoding" "utf-8"],
      pure $ Doctype "DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\""
    ]

-- ============================================================================
-- Character predicates (local, replacing mpar imports)
-- ============================================================================

isWhitespace :: Char -> Bool
isWhitespace ' ' = True
isWhitespace '\n' = True
isWhitespace '\t' = True
isWhitespace '\r' = True
isWhitespace _ = False

-- ============================================================================
-- Token parsers (Circuit.Parser, String-based)
-- ============================================================================

-- | capture consumed chars and convert to ByteString
bs :: Parser String Char a -> Parser String Char ByteString
bs p = B.pack . fst <$> captured p

-- | equals sign with optional whitespace
eq_ :: Parser String Char ()
eq_ = skipWhile isWhitespace *> char '=' *> skipWhile isWhitespace

-- | quoted string: single or double quoted
wrappedQ :: Parser String Char ByteString
wrappedQ =
  (char '\'' *> bs (many (satisfy (/= '\''))) <* char '\'')
    <|> (char '"' *> bs (many (satisfy (/= '"'))) <* char '"')

tokenXmlP :: Parser String Char Token
tokenXmlP =
  (string "<!--" *> commentP_)
    <|> (string "<!" *> doctypeXmlP_)
    <|> (string "</" *> endTagXmlP_)
    <|> (string "<?" *> declXmlP_)
    <|> (string "<" *> startTagsXmlP_)
    <|> contentP_

tokenHtmlP :: Parser String Char Token
tokenHtmlP =
  (string "<!--" *> commentP_)
    <|> (string "<!" *> doctypeHtmlP_)
    <|> (string "</" *> endTagHtmlP_)
    <|> (string "<?" *> bogusCommentHtmlP_)
    <|> (string "<" *> startTagsHtmlP_)
    <|> contentP_

-- XML name start char (production [4])
isNameStartChar :: Char -> Bool
isNameStartChar x =
  isLatinLetter x
    || x == ':'
    || x == '_'
    || (x >= '\xC0' && x <= '\xD6')
    || (x >= '\xD8' && x <= '\xF6')
    || (x >= '\xF8' && x <= '\xFF')

-- XML/HMTL name char
isNameChar :: Char -> Bool
isNameChar x = not (isWhitespace x || x == '/' || x == '<' || x == '>')

isNameCharXml :: Char -> Bool
isNameCharXml x =
  isLatinLetter x
    || Data.Char.isDigit x
    || x `elem` (":_-.·" :: String)
    || (x >= '\xC0' && x <= '\xD6')
    || (x >= '\xD8' && x <= '\xF6')
    || (x >= '\xF8' && x <= '\xFF')

isAttrName :: Char -> Bool
isAttrName x = not (isWhitespace x || x == '/' || x == '>' || x == '=' || x == '<')

isBooleanAttrName :: Char -> Bool
isBooleanAttrName x = not (isWhitespace x || x == '/' || x == '>' || x == '<')

-- XML parsers

nameStartCharXmlP :: Parser String Char Char
nameStartCharXmlP = satisfy isNameStartChar

nameCharXmlP :: Parser String Char Char
nameCharXmlP = satisfy isNameCharXml

nameXmlP :: Parser String Char ByteString
nameXmlP = bs (nameStartCharXmlP *> many nameCharXmlP)

commentP_ :: Parser String Char Token
commentP_ = Comment <$> (bs (many (satisfy (/= '-') <|> (char '-' *> satisfy (/= '-')))) <* string "-->")

contentP_ :: Parser String Char Token
contentP_ = Content <$> bs (some (satisfy (/= '<')))

declXmlP_ :: Parser String Char Token
declXmlP_ =
  let attr key = Attr (B.pack key) <$> (skipWhile isWhitespace *> string key *> eq_ *> wrappedQ)
      one x = [x]
   in string "xml"
        *> (Decl "xml" <$> ((:) <$> attr "version" <*> (one <$> attr "encoding")))
        <* skipWhile isWhitespace
        <* string "?>"

doctypeXmlP_ :: Parser String Char Token
doctypeXmlP_ =
  Doctype
    <$> ( bs
            ( string "DOCTYPE"
                *> skipWhile isWhitespace
                *> void nameXmlP
                *> skipWhile isWhitespace
                *> many (satisfy (/= '>'))
            )
            <* char '>'
        )

startTagsXmlP_ :: Parser String Char Token
startTagsXmlP_ =
  OpenTag EmptyElemTag
    <$> (nameXmlP <* skipWhile isWhitespace <* string "/>")
    <*> pure []
      <|> OpenTag StartTag
    <$> (nameXmlP <* skipWhile isWhitespace <* string ">")
    <*> many (skipWhile isWhitespace *> attrXmlP_)

attrXmlP_ :: Parser String Char Attr
attrXmlP_ = Attr <$> (nameXmlP <* eq_) <*> wrappedQ

endTagXmlP_ :: Parser String Char Token
endTagXmlP_ = EndTag <$> (nameXmlP <* skipWhile isWhitespace <* char '>')

-- HTML parsers

nameHtmlP :: Parser String Char ByteString
nameHtmlP = bs (satisfy isLatinLetter *> many (satisfy isNameChar))

startTagsHtmlP_ :: Parser String Char Token
startTagsHtmlP_ =
  OpenTag StartTag
    <$> (nameHtmlP <* skipWhile isWhitespace)
    <*> (attrsHtmlP_ <* skipWhile isWhitespace <* string ">")
      <|> OpenTag EmptyElemTag
    <$> (nameHtmlP <* skipWhile isWhitespace)
    <*> (attrsHtmlP_ <* skipWhile isWhitespace <* string "/>")

endTagHtmlP_ :: Parser String Char Token
endTagHtmlP_ = EndTag <$> (nameHtmlP <* skipWhile isWhitespace <* char '>')

attrHtmlP_ :: Parser String Char Attr
attrHtmlP_ =
  (Attr <$> (bs (many (satisfy isAttrName)) <* eq_) <*> (wrappedQ <|> bs (some (satisfy isBooleanAttrName))))
    <|> (flip Attr B.empty <$> bs (some (satisfy isBooleanAttrName)))

attrsHtmlP_ :: Parser String Char [Attr]
attrsHtmlP_ = many (skipWhile isWhitespace *> attrHtmlP_) <* skipWhile isWhitespace

doctypeHtmlP_ :: Parser String Char Token
doctypeHtmlP_ =
  Doctype
    <$> ( bs
            ( string "DOCTYPE"
                *> skipWhile isWhitespace
                *> void nameHtmlP
                *> skipWhile isWhitespace
            )
            <* char '>'
        )

bogusCommentHtmlP_ :: Parser String Char Token
bogusCommentHtmlP_ = Comment <$> bs (some (satisfy (/= '<')))

-- | Parse a tag name.
nameP :: Standard -> Parser String Char ByteString
nameP Html = nameHtmlP
nameP Xml = nameXmlP

-- | Parse an attribute.
-- | Parse attributes list.
attrsP :: Standard -> Parser String Char [Attr]
attrsP Html = attrsHtmlP_
attrsP Xml = many (skipWhile isWhitespace *> attrXmlP_) <* skipWhile isWhitespace

-- | Alias for single whitespace (backward compat with mpar)
ws :: Parser String Char Char
ws = satisfy isWhitespace

-- | Alias for skip whitespace (backward compat with mpar)
ws_ :: Parser String Char ()
ws_ = skipWhile isWhitespace

-- | Run parser, returning leftovers and errors as 'ParserWarning's.
--
-- >>> runParserWarn ws " "
-- That ' '
--
-- >>> runParserWarn ws "x"
-- This ParserUncaught
--
-- >>> runParserWarn ws " x"
-- These (ParserLeftover "x") ' '
runParserWarn :: Parser String Char a -> String -> These ParserWarning a
runParserWarn p s = case CP.runParser p s of
  These a "" -> That a
  These a rest -> These (ParserLeftover (take 200 rest)) a
  This a -> That a
  That _ -> This ParserUncaught

-- | Run a parser and return the remaining input and result as a tuple
runMarkupParser :: Parser String Char a -> String -> (String, These () a)
runMarkupParser p s = case CP.runParser p s of
  These a s' -> (s', These () a)
  This a -> ([], That a)
  That s' -> (s', This ())

runParser_ :: Parser String Char a -> String -> a
runParser_ p s = case CP.runParser p s of
  These a _ -> a
  This a -> a
  That _ -> error "Uncaught parse failure"
