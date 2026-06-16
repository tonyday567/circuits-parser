

-- | Bridge: the fast imperative Lexer → structured Markup Tokens.
--
-- The Lexer ('Circuit.Parser.Lexer') produces a flat stream of
-- 'MarkupToken's: @TOpenTag \"foo\"@, @TAttrName \"class\"@,
-- @TAttrVal \"bar\"@, @TTagEnd@ are four separate tokens.
--
-- 'groupTokens' accumulates consecutive attribute tokens into a
-- single @OpenTag StartTag name attrs@, mapping the flat stream
-- into 'Circuit.Markup.Token's that 'Circuit.Markup.gather' can consume.
--
-- Limitations inherited from the Lexer:
--
--   * Comments (@<!-- -->@) are not recognised — the Lexer treats
--     them as malformed open tags.
--   * Doctypes (@<!DOCTYPE ...>@) are not handled.
--   * Self-closing tags without a space before @\/@ (e.g. @<br/>@)
--     may include the slash in the tag name.
--
-- For full-featured parsing, prefer 'Circuit.Markup.tokenize' which
-- uses the compositional 'Circuit.Parser' backend.
module Circuit.Parser.Lexer.Bridge
  ( groupTokens,
  )
where

import Circuit.Markup (Attr (..), OpenTagType (..), Token (..))
import Circuit.Parser.Lexer (MarkupToken (..))
import Data.ByteString (ByteString)

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Circuit.Parser.Lexer (MarkupToken (..))
-- >>> import Circuit.Parser.Lexer.Bridge

-- | Accumulate a flat 'MarkupToken' stream into structured 'Token's.
--
-- >>> groupTokens [TOpenTag "foo", TAttrName "class", TAttrVal "bar", TTagEnd, TContent "hello", TCloseTag "foo"]
-- [OpenTag StartTag "foo" [Attr {attrName = "class", attrValue = "bar"}],Content "hello",EndTag "foo"]
groupTokens :: [MarkupToken] -> [Token]
groupTokens = go
  where
    go [] = []
    go (TOpenTag name : rest) =
      let (attrs, rest') = collectAttrs rest
       in case rest' of
            (TSelfClose : rest'') ->
              OpenTag EmptyElemTag name (map toAttr attrs) : go rest''
            (TTagEnd : rest'') ->
              OpenTag StartTag name (map toAttr attrs) : go rest''
            _ ->
              -- Unterminated open tag: emit what we have, continue
              OpenTag StartTag name (map toAttr attrs) : go rest'
    go (TCloseTag name : rest) = EndTag name : go rest
    go (TContent bytes : rest) = Content bytes : go rest
    go (TComment bytes : rest) = Comment bytes : go rest
    -- Stray attribute tokens without a preceding open tag: skip
    go (TAttrName _ : rest) = go rest
    go (TAttrVal _ : rest) = go rest
    go (TTagEnd : rest) = go rest
    go (TSelfClose : rest) = go rest

toAttr :: (ByteString, ByteString) -> Attr
toAttr (n, v) = Attr n v

-- | Collect consecutive @TAttrName@/@TAttrVal@ pairs.
collectAttrs :: [MarkupToken] -> ([(ByteString, ByteString)], [MarkupToken])
collectAttrs = go []
  where
    go acc (TAttrName n : TAttrVal v : rest) =
      go ((n, v) : acc) rest
    go acc rest = (reverse acc, rest)
