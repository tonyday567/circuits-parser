{-# LANGUAGE GHC2024 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Tree operations for markup.
module Circuit.Markup.Tree
  ( gather,
    gather_,
    degather,
    degather_,
    normalize,
    normContent,
    wellFormed,
    isWellFormed,
    selfClosers,
    element,
    element_,
    emptyElem,
    elementc,
    contentRaw,
    addAttrs,
  )
where

import Circuit.Markup.Types
import Circuit.Markup.Warn (concatWarns, showWarnings, warnError)
import Control.Category ((>>>))
import Data.Bifunctor
import Data.Bool
import Data.ByteString (ByteString)
import Data.Function
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.These
import Data.Tree

-- $setup
-- >>> :set -XOverloadedStrings

-- | Append attributes to an existing Token attribute list. Returns Nothing for tokens that do not have attributes.
addAttrs :: [Attr] -> Token -> Maybe Token
addAttrs as (OpenTag t n as') = Just $ OpenTag t n (as <> as')
addAttrs _ _ = Nothing

-- | Html tags that self-close
selfClosers :: [NameTag]
selfClosers =
  [ "area",
    "base",
    "br",
    "col",
    "embed",
    "hr",
    "img",
    "input",
    "link",
    "meta",
    "param",
    "source",
    "track",
    "wbr"
  ]

-- | Create 'Markup' from a name tag and attributes that wraps some other markup.
--
-- >>> element "div" [] (element_ "br" [])
-- Markup {elements = [Node {rootLabel = OpenTag StartTag "div" [], subForest = [Node {rootLabel = OpenTag StartTag "br" [], subForest = []}]}]}
element :: NameTag -> [Attr] -> Markup -> Markup
element n as (Markup xs) = Markup [Node (OpenTag StartTag n as) xs]

-- | Create 'Markup' from a name tag and attributes that doesn't wrap some other markup. The 'OpenTagType' used is 'StartTag'. Use 'emptyElem' if you want to create 'EmptyElemTag' based markup.
--
-- >>> (element_ "br" [])
-- Markup {elements = [Node {rootLabel = OpenTag StartTag "br" [], subForest = []}]}
element_ :: NameTag -> [Attr] -> Markup
element_ n as = Markup [Node (OpenTag StartTag n as) []]

-- | Create 'Markup' from a name tag and attributes using 'EmptyElemTag', that doesn't wrap some other markup. No checks are made on whether this creates well-formed markup.
--
-- >>> emptyElem "br" []
-- Markup {elements = [Node {rootLabel = OpenTag EmptyElemTag "br" [], subForest = []}]}
emptyElem :: NameTag -> [Attr] -> Markup
emptyElem n as = Markup [Node (OpenTag EmptyElemTag n as) []]

-- | Create 'Markup' from a name tag and attributes that wraps some 'Content'. No escaping is performed.
--
-- >>> elementc "div" [] "content"
-- Markup {elements = [Node {rootLabel = OpenTag StartTag "div" [], subForest = [Node {rootLabel = Content "content", subForest = []}]}]}
elementc :: NameTag -> [Attr] -> ByteString -> Markup
elementc n as b = element n as (contentRaw b)

-- | Create a Markup element from a bytestring, not escaping the usual characters.
--
-- >>> contentRaw "<content>"
-- Markup {elements = [Node {rootLabel = Content "<content>", subForest = []}]}
contentRaw :: ByteString -> Markup
contentRaw b = Markup [pure $ Content b]

normTokenAttrs :: Token -> Token
normTokenAttrs (OpenTag t n as) = OpenTag t n (normAttrs as)
normTokenAttrs x = x

-- | normalize an attribution list, removing duplicate AttrNames, and space concatenating class values.
normAttrs :: [Attr] -> [Attr]
normAttrs as =
  uncurry Attr
    <$> Map.toList
      ( foldl'
          ( \s (Attr n v) ->
              Map.insertWithKey
                ( \k new old ->
                    case k of
                      "class" -> old <> " " <> new
                      _ -> new
                )
                n
                v
                s
          )
          Map.empty
          as
      )

-- | Concatenate sequential content and normalize attributes; unwording class values and removing duplicate attributes (taking last).
normalize :: Markup -> Markup
normalize m = normContent $ Markup $ fmap (fmap normTokenAttrs) (elements m)

-- | Are the trees in the markup well-formed?
isWellFormed :: Standard -> Markup -> Bool
isWellFormed s = (== []) . wellFormed s

-- | Check for well-formedness and return warnings encountered.
--
-- >>> wellFormed Html $ Markup [Node (Comment "") [], Node (EndTag "foo") [], Node (OpenTag EmptyElemTag "foo" []) [Node (Content "bar") []], Node (OpenTag EmptyElemTag "foo" []) []]
-- [EmptyContent,EndTagInTree,LeafWithChildren,BadEmptyElemTag]
wellFormed :: Standard -> Markup -> [MarkupWarning]
wellFormed s (Markup trees) = List.nub $ mconcat (foldTree checkNode <$> trees)
  where
    checkNode (OpenTag StartTag _ _) xs = mconcat xs
    checkNode (OpenTag EmptyElemTag n _) [] =
      bool [] [BadEmptyElemTag] (notElem n selfClosers && s == Html)
    checkNode (EndTag _) [] = [EndTagInTree]
    checkNode (Content b) [] = bool [] [EmptyContent] (b == "")
    checkNode (Comment b) [] = bool [] [EmptyContent] (b == "")
    checkNode (Decl b as) []
      | b == "" = [EmptyContent]
      | s == Html && as /= [] = [BadDecl]
      | s == Xml && ("version" `elem` (attrName <$> as)) && ("encoding" `elem` (attrName <$> as)) =
          [BadDecl]
      | otherwise = []
    checkNode (Doctype b) [] = bool [] [EmptyContent] (b == "")
    checkNode _ _ = [LeafWithChildren]

-- | Normalise Content in Markup, concatenating adjacent Content, and removing mempty Content.
normContent :: Markup -> Markup
normContent (Markup trees) = Markup $ foldTree (\x xs -> Node x (filter ((/= Content "") . rootLabel) $ concatContent xs)) <$> concatContent trees

concatContent :: [Tree Token] -> [Tree Token]
concatContent = \case
  ((Node (Content t) _) : (Node (Content t') _) : ts) -> concatContent $ Node (Content (t <> t')) [] : ts
  (t : ts) -> t : concatContent ts
  [] -> []

-- | Gather together token trees from a token list, placing child elements in nodes and removing EndTags.
gather :: Standard -> TokenParser [MarkupWarning] Markup
gather s = TokenParser $ \ts ->
  let (Cursor finalSibs finalParents, warnings) =
        foldl' (\(c, xs) t -> incCursor s t c & second (maybeToList >>> (<> xs))) (Cursor [] [], []) ts
   in case (finalSibs, finalParents, warnings) of
        (sibs, [], []) -> ([], That (Markup (reverse sibs)))
        ([], [], xs) -> ([], This xs)
        (sibs, ps, xs) ->
          let result = reverse $ foldl' (\ss' (p, ss) -> Node p (reverse ss') : ss) sibs ps
           in ([], These (xs <> [UnclosedTag]) (Markup result))

-- | 'gather' but errors on warnings.
gather_ :: Standard -> [Token] -> Markup
gather_ s ts = case runTP (gather s) ts of
  ([], That m) -> m
  ([], This w) -> error (showWarnings w)
  ([], These w m) -> if null w then m else error (showWarnings w)
  _ -> error "Impossible: gather should consume all tokens"

incCursor :: Standard -> Token -> Cursor -> (Cursor, Maybe MarkupWarning)
-- Only StartTags are ever pushed on to the parent list, here:
incCursor Xml t@(OpenTag StartTag _ _) (Cursor ss ps) = (Cursor [] ((t, ss) : ps), Nothing)
incCursor Html t@(OpenTag StartTag n _) (Cursor ss ps) =
  (bool (Cursor [] ((t, ss) : ps)) (Cursor (Node t [] : ss) ps) (n `elem` selfClosers), Nothing)
incCursor Xml t@(OpenTag EmptyElemTag _ _) (Cursor ss ps) = (Cursor (Node t [] : ss) ps, Nothing)
incCursor Html t@(OpenTag EmptyElemTag n _) (Cursor ss ps) =
  ( Cursor (Node t [] : ss) ps,
    bool (Just BadEmptyElemTag) Nothing (n `elem` selfClosers)
  )
incCursor _ (EndTag n) (Cursor ss ((p@(OpenTag StartTag n' _), ss') : ps)) =
  ( Cursor (Node p (reverse ss) : ss') ps,
    bool (Just (TagMismatch n n')) Nothing (n == n')
  )
-- Non-StartTag on parent list
incCursor _ (EndTag _) (Cursor ss ((p, ss') : ps)) =
  ( Cursor (Node p (reverse ss) : ss') ps,
    Just LeafWithChildren
  )
incCursor _ (EndTag _) (Cursor ss []) =
  ( Cursor ss [],
    Just UnmatchedEndTag
  )
incCursor _ t (Cursor ss ps) = (Cursor (Node t [] : ss) ps, Nothing)

data Cursor = Cursor
  { -- siblings, not (yet) part of another element
    _sibs :: [Tree Token],
    -- open elements and their siblings.
    _stack :: [(Token, [Tree Token])]
  }

-- | Convert a markup into a token list, adding end tags.
degather :: Standard -> Markup -> Warn [Token]
degather s (Markup tree) = concatWarns $ foldTree (addCloseTags s) <$> tree

-- | 'degather' but errors on warning
degather_ :: Standard -> Markup -> [Token]
degather_ s m = degather s m & warnError

addCloseTags :: Standard -> Token -> [Warn [Token]] -> Warn [Token]
addCloseTags std s@(OpenTag StartTag n _) children
  | children /= [] && n `elem` selfClosers && std == Html =
      These [SelfCloserWithChildren] [s] <> concatWarns children
  | n `elem` selfClosers && std == Html =
      That [s] <> concatWarns children
  | otherwise =
      That [s] <> concatWarns children <> That [EndTag n]
addCloseTags _ x xs = case xs of
  [] -> That [x]
  cs -> These [LeafWithChildren] [x] <> concatWarns cs
