{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A 'Markup' parser and printer of strict 'ByteString's focused on optimising performance. 'Markup' is a representation of data such as HTML, SVG or XML but the parsing is not always at standards.
module Circuit.Markup
  ( module Circuit.Markup.Types,
    module Circuit.Markup.Parse,
    module Circuit.Markup.Render,
    module Circuit.Markup.Tree,
    module Circuit.Markup.Warn,
    -- * parsing
    Parser,
    -- * Tree support
    Tree (..),
  )
where

import Circuit.Markup.Parse
import Circuit.Markup.Render
import Circuit.Markup.Tree
import Circuit.Markup.Types
import Circuit.Markup.Warn
import Circuit.Parser (Parser)
import Data.Tree (Tree (..))

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString.Char8 qualified as B
-- >>> import Data.Tree
-- >>> import Circuit.Parser (many)

-- $usage
--
-- > import Circuit.Markup
-- > import Data.ByteString qualified as B
-- >
-- > bs <- B.readFile "other/line.svg"
-- > m = markup_ bs
--
-- @'markdown_' . 'markup_'@ is an isomorphic round trip from 'Markup' to 'ByteString' to 'Markup':
--
-- - This is subject to the 'Markup' being 'wellFormed'.
--
-- - The round-trip @'markup_' . 'markdown_'@ is not isomorphic as parsing forgets whitespace within tags, comments and declarations.
--
-- - The underscores represent versions of main functions that throw an exception on warnings encountered along the way.
--
-- At a lower level, a round trip pipeline might look something like:
--
-- > tokenize Html >=>
--
-- - 'tokenize' converts a 'ByteString' to a 'Token' list.
--
-- > gather Html >=>
--
-- - 'gather' takes the tokens and gathers them into 'Tree's of 'Token's which is what 'Markup' is.
--
-- > (normalize >>> pure) >=>
--
-- - 'normalize' concatenates content, and normalizes attributes,
--
-- > degather Html >=>
--
-- - 'degather' turns the markup tree back into a token list. Finally,
--
-- > fmap (detokenize Html) >>> pure
--
-- - 'detokenize' turns a token back into a bytestring.
--
-- Along the way, the kleisi fishies and compose forward usage accumulates any warnings via the 'These' monad instance, which is wrapped into a type synonym named 'Warn'.

-- * Types

-- | A list of 'Element's or 'Tree' 'Token's
--
-- >>> markup Html "<foo class=\"bar\">baz</foo>"
-- That (Markup {elements = [Node {rootLabel = OpenTag StartTag "foo" [Attr {attrName = "class", attrValue = "bar"}], subForest = [Node {rootLabel = Content "baz", subForest = []}]}]})

-- | A Markup token. The term is borrowed from <https://www.w3.org/html/wg/spec/tokenization.html#tokenization HTML> standards but is used across 'Html' and 'Xml' in this library.
--
-- >>> runParser_ (many (tokenP Html)) "<foo>content</foo>"
-- [OpenTag StartTag "foo" [],Content "content",EndTag "foo"]
--
-- >>> runParser_ (tokenP Xml) "<foo/>"
-- OpenTag EmptyElemTag "foo" []
--
-- >>> runParser_ (tokenP Html) "<!-- Comment -->"
-- Comment " Comment "
--
-- >>> runParser_ (tokenP Xml) "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
-- Decl "xml" [Attr {attrName = "version", attrValue = "1.0"},Attr {attrName = "encoding", attrValue = "UTF-8"}]
--
-- >>> runParser_ (tokenP Html) "<!DOCTYPE html>"
-- Doctype "DOCTYPE html"
--
-- >>> runParser_ (tokenP Xml) "<!DOCTYPE foo [ declarations ]>"
-- Doctype "DOCTYPE foo [ declarations ]"
--
-- >>> runMarkupParser (tokenP Html) "<foo a=\"a\" b=\"b\" c=c check>"
-- ("",These () (OpenTag StartTag "foo" [Attr {attrName = "a", attrValue = "a"},Attr {attrName = "b", attrValue = "b"},Attr {attrName = "c", attrValue = "c"},Attr {attrName = "check", attrValue = ""}]))
--
-- >>> runMarkupParser (tokenP Xml) "<foo a=\"a\" b=\"b\" c=c check>"
-- ("<foo a=\"a\" b=\"b\" c=c check>",This ())

-- | An attribute of a tag
--
-- >>> detokenize Html <$> tokenize_ Html "<input checked>"
-- ["<input checked=\"\">"]

-- * Warnings

-- | Convert any warnings to an 'error'
--
-- >>> warnError $ (tokenize Html) "<foo"
-- *** Exception: MarkupParser (ParserLeftover "<foo")
-- ...

-- | Returns Left on any warnings
--
-- >>> warnEither $ (tokenize Html) "<foo><baz"
-- Left [MarkupParser (ParserLeftover "<baz")]

-- | Returns results, if any, ignoring warnings.
--
-- >>> warnMaybe $ (tokenize Html) "<foo><baz"
-- Just [OpenTag StartTag "foo" []]

-- * Parse

-- | Standard Html Doctype
--
-- >>> markdown_ Compact Html doctypeHtml
-- "<!DOCTYPE html>"

-- | Standard Xml Doctype
--
-- >>> markdown_ Compact Xml doctypeXml
-- "<?xml version=\"1.0\" encoding=\"utf-8\"?><!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n    \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"

-- * Render

-- | Convert 'Markup' to bytestrings
--
-- >>> markdown (Indented 4) Html (markup_ Html "<foo><br></foo>")
-- That "<foo>\n    <br>\n</foo>"

-- | Convert 'Markup' to 'ByteString' and error on warnings.
--
-- >>> B.putStr $ markdown_ (Indented 4) Html (markup_ Html "<foo><br></foo>")
-- <foo>
--     <br>
-- </foo>

-- * Tree

-- | Concatenate sequential content and normalize attributes; unwording class values and removing duplicate attributes (taking last).
--
-- >>> B.putStr $ warnError $ markdown Compact Html $ normalize (markup_ Html "<foo bar=\"first\"></foo>")
-- <foo bar="first"></foo>

-- | Normalise Content in Markup, concatenating adjacent Content, and removing mempty Content.
--
-- >>> normContent $ content "a" <> content "" <> content "b"
-- Markup {elements = [Node {rootLabel = Content "ab", subForest = []}]}

-- | Gather together token trees from a token list, placing child elements in nodes and removing EndTags.
--
-- >>> gather_ Html (tokenize_ Html "<foo class=\"bar\">baz</foo>")
-- Markup {elements = [Node {rootLabel = OpenTag StartTag "foo" [Attr {attrName = "class", attrValue = "bar"}], subForest = [Node {rootLabel = Content "baz", subForest = []}]}]}

-- | Convert a markup into a token list, adding end tags.
--
-- >>> degather Html =<< markup Html "<foo class=\"bar\">baz</foo>"
-- That [OpenTag StartTag "foo" [Attr {attrName = "class", attrValue = "bar"}],Content "baz",EndTag "foo"]

-- | Create a Markup element from a bytestring, not escaping the usual characters.
--
-- >>> markup_ Html $ markdown_ Compact Html $ contentRaw "<content>"
-- *** Exception: UnclosedTag
-- ...
