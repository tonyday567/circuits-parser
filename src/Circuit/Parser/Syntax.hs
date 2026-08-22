{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Reifiable parser syntax.
--
-- The parser type in "Circuit.Parser" hides primitive operations inside
-- @K m@ closures over a @Body (,) (K m) f@ base. This module
-- exposes those primitives as constructors in a syntax tree, so the same
-- parser can be executed /and/ analyzed.
--
-- The plumbing — sequential composition and the structural combinators — is
-- the free category over the pure ambient-state arrow @SArr f@
-- (@Body (,) (->) f@), extended with 'SigPrim' and 'SigComb' signatures.
-- The stream @f@ is ambient state throughout: primitives consume it, and
-- choice is a structural combinator, not a trace. There is no 'SigKnot' /
-- @Loop Either@ here — the knot-body category @Body@ is the fold target,
-- and the stream is never hidden in a feedback channel.
--
-- Executing a syntax tree is an algebra fold into @Body (,) (K m) f@;
-- static analysis is a fold into other targets ('FirstSet', 'Regex', the
-- Brzozowski derivative).
--
-- === doctests
--
-- >>> import Data.Functor.Identity (Identity)
-- >>> import Data.These (These(..))
-- >>> import Control.Applicative ((<|>))
-- >>> import Circuit.Parser.Syntax (charS, stringS, manyS, firstSet, toRegex)
--
-- >>> runParserSyntaxIdentity (charS 'a') "abc"
-- These 'a' "bc"
--
-- >>> runParserSyntaxIdentity (stringS "ab" <|> stringS "a") "ab"
-- These "ab" ""
--
-- >>> runParserSyntaxIdentity (stringS "ab" <|> stringS "a") "a"
-- These "a" ""
--
-- >>> runParserSyntaxIdentity (manyS (charS 'a')) "aaab"
-- These "aaa" "b"
--
-- >>> firstSet (charS 'a' <|> charS 'b' :: ParserSyntax String Char Char)
-- FirstSet {nullable = False, firstKind = FSPred <function>}
--
-- >>> toRegex (manyS (charS 'a') :: ParserSyntax String Char String)
-- Just REStar (REChar 'a')
module Circuit.Parser.Syntax
  ( -- * Signatures
    SigPrim (..),
    SigComb (..),

    -- * Syntax tree
    ParserSyntax (..),

    -- * Primitive constructors
    nextS,
    anyTokenS,
    satisfyS,
    charS,
    stringS,
    endOfInputS,
    takeRestS,

    -- * Combinator constructors
    tryS,
    optionalS,
    manyS,
    someS,
    skipManyS,
    countS,
    sepByS,
    sepBy1S,
    withOptionS,

    -- * Execution
    runParserSyntax,
    runParserSyntaxIdentity,

    -- * Static analysis
    FirstSet (..),
    firstSet,
    unreachableBranches,

    -- * Regex extraction
    Regex (..),
    toRegex,

    -- * Brzozowski derivatives
    derive,
    nullableValue,
  )
where

import Circuit.Body (Body (..), SArr (..))
import Circuit.Category (Category (..), K (..))
import Circuit.Fragment
  ( Algebra (..),
    SigCompose (..),
    Syntax (..),
    evalInto,
    (:+:) (..),
  )
import Circuit.Parser (Parser (..), Uncons (..))
import Circuit.Parser qualified as PU
import Control.Applicative (Alternative (empty, (<|>)), optional)
import Control.Monad (MonadPlus, void)
import Data.Kind (Type)
import Data.These (These (..))
import Prelude hiding (id, (.))

-- $setup
-- >>> import Data.Functor.Identity (Identity)
-- >>> import Data.These (These (..))
-- >>> import Control.Applicative ((<|>))
-- >>> import Circuit.Parser.Syntax (charS, stringS, manyS, firstSet, toRegex)

-- ---------------------------------------------------------------------------
-- Signatures
-- ---------------------------------------------------------------------------

-- | One-step parser primitives. Each constructor names a leaf operation on a
-- stream of elements @s@ with stream type @f@. The stream is ambient state,
-- so the source object is unit and the target carries the result plus
-- leftover stream.
data SigPrim (f :: Type) (s :: Type) (arr :: Type -> Type -> Type) (rec :: Type -> Type -> Type) (a :: Type) (b :: Type) where
  PrimNext :: SigPrim f s arr rec () (These s f)
  PrimSatisfy :: (s -> Bool) -> SigPrim f s arr rec () (These s f)
  PrimChar :: (Eq s) => s -> SigPrim f s arr rec () (These s f)
  PrimString :: (Eq s) => [s] -> SigPrim f s arr rec () (These [s] f)
  PrimEndOfInput :: SigPrim f s arr rec () (These () f)
  PrimTakeRest :: SigPrim f s arr rec () (These f f)

-- | Structural combinators that cannot be expressed as pure sequential
-- composition while preserving the syntax tree. These are eliminated by the
-- execution algebra into the corresponding @Parser@ combinators.
data SigComb (f :: Type) (s :: Type) (arr :: Type -> Type -> Type) (rec :: Type -> Type -> Type) (a :: Type) (b :: Type) where
  CombAp :: rec () (These (a -> b) f) -> rec () (These a f) -> SigComb f s arr rec () (These b f)
  CombBind :: rec () (These a f) -> (a -> rec () (These b f)) -> SigComb f s arr rec () (These b f)
  CombAlt :: rec () (These a f) -> rec () (These a f) -> SigComb f s arr rec () (These a f)
  CombMany :: rec () (These a f) -> SigComb f s arr rec () (These [a] f)
  CombTry :: rec () (These a f) -> SigComb f s arr rec () (These a f)
  CombFmap :: (a -> b) -> rec () (These a f) -> SigComb f s arr rec () (These b f)

-- | The full parser signature: composition, primitives, and structural
-- combinators. No 'SigKnot' — choice is a combinator, not a trace.
type ParserSyntaxSig f s = SigCompose :+: SigPrim f s :+: SigComb f s

-- | A parser syntax tree with stream type @f@, element type @s@, and result
-- type @a@. The stream is ambient state (@SArr f@ base arrow), the source
-- object is unit, and the target carries the result plus leftover stream.
newtype ParserSyntax (f :: Type) (s :: Type) (a :: Type) = ParserSyntax
  { unParserSyntax ::
      Syntax (ParserSyntaxSig f s) (SArr f) () (These a f)
  }

-- | The underlying syntax type supports sequential composition via 'SigCompose'.
instance Category (Syntax (ParserSyntaxSig f s) (SArr f)) where
  id = Lift id
  f . g = Op (L (SigCompose f g))

-- ---------------------------------------------------------------------------
-- Algebra for execution
-- ---------------------------------------------------------------------------

-- | Map primitive operations to their implementations in
-- @Body (,) (K m) f@.
instance
  (Monad m, Uncons f s) =>
  Algebra (SigPrim f s) (SArr f) (Body (,) (K m) f)
  where
  type
    Ctx (SigPrim f s) (SArr f) (Body (,) (K m) f) =
      (Monad m, Uncons f s)
  alg _ _ PrimNext = unParser (PU.next @m @f @s)
  alg _ _ (PrimSatisfy p) = unParser (PU.satisfy @m @f @s p)
  alg _ _ (PrimChar c) = unParser (PU.char @m @f @s c)
  alg _ _ (PrimString cs) = unParser (PU.string @m @f @s cs)
  alg _ _ PrimEndOfInput = unParser (PU.endOfInput @m @f @s)
  alg _ _ PrimTakeRest = unParser (PU.takeRest @m @f @s)

instance
  (Monad m, Uncons f s) =>
  Algebra (SigComb f s) (SArr f) (Body (,) (K m) f)
  where
  type
    Ctx (SigComb f s) (SArr f) (Body (,) (K m) f) =
      (Monad m, Uncons f s)
  alg _ rec (CombAp pf pa) = unParser (Parser @m @f @s (rec pf) <*> Parser @m @f @s (rec pa))
  alg _ rec (CombBind p k) = unParser (Parser @m @f @s (rec p) >>= \a -> Parser @m @f @s (rec (k a)))
  alg _ rec (CombAlt p1 p2) = unParser (Parser @m @f @s (rec p1) <|> Parser @m @f @s (rec p2))
  alg _ rec (CombMany p) = unParser (PU.many @m @f @s (Parser @m @f @s (rec p)))
  alg _ rec (CombTry p) = unParser (PU.try @m @f @s (Parser @m @f @s (rec p)))
  alg _ rec (CombFmap g p) = unParser (g <$> Parser @m @f @s (rec p))

-- | Interpret syntax into a concrete parser.
runParserSyntax ::
  forall m f s a.
  (Monad m, Uncons f s) =>
  ParserSyntax f s a ->
  Parser m f s a
runParserSyntax (ParserSyntax syn) = Parser (evalInto emb syn)
  where
    emb :: forall x y. SArr f x y -> Body (,) (K m) f x y
    emb (SArr g) = Body $ K (pure . g)

-- | Interpret syntax into an identity parser.
runParserSyntaxIdentity ::
  (Uncons f s) =>
  ParserSyntax f s a ->
  f ->
  These a f
runParserSyntaxIdentity p = PU.runParserIdentity (runParserSyntax p)

-- ---------------------------------------------------------------------------
-- Instances
-- ---------------------------------------------------------------------------

instance (Uncons f s) => Functor (ParserSyntax f s) where
  fmap g (ParserSyntax p) =
    ParserSyntax (Op (R (R (CombFmap g p))))

instance (Uncons f s) => Applicative (ParserSyntax f s) where
  pure a = ParserSyntax $ Lift $ SArr $ \(st, ()) -> (st, These a st)
  ParserSyntax pf <*> ParserSyntax pa =
    ParserSyntax $ Op $ R $ R $ CombAp pf pa

instance (Uncons f s) => Monad (ParserSyntax f s) where
  ParserSyntax m >>= k =
    ParserSyntax $
      Op $
        R $
          R $
            CombBind m (unParserSyntax . k)

instance (Uncons f s) => Alternative (ParserSyntax f s) where
  empty = ParserSyntax $ Lift $ SArr $ \(st, ()) -> (st, That st)
  ParserSyntax p1 <|> ParserSyntax p2 =
    ParserSyntax $ Op $ R $ R $ CombAlt p1 p2

instance (Uncons f s) => MonadPlus (ParserSyntax f s)

-- ---------------------------------------------------------------------------
-- Primitive constructors
-- ---------------------------------------------------------------------------

nextS :: ParserSyntax f s s
nextS = ParserSyntax $ Op $ R $ L PrimNext

anyTokenS :: ParserSyntax f s s
anyTokenS = nextS

satisfyS :: (s -> Bool) -> ParserSyntax f s s
satisfyS p = ParserSyntax $ Op $ R $ L (PrimSatisfy p)

charS :: (Eq s) => s -> ParserSyntax f s s
charS c = ParserSyntax $ Op $ R $ L (PrimChar c)

stringS :: (Eq s) => [s] -> ParserSyntax f s [s]
stringS cs = ParserSyntax $ Op $ R $ L (PrimString cs)

endOfInputS :: ParserSyntax f s ()
endOfInputS = ParserSyntax $ Op $ R $ L PrimEndOfInput

takeRestS :: ParserSyntax f s f
takeRestS = ParserSyntax $ Op $ R $ L PrimTakeRest

-- ---------------------------------------------------------------------------
-- Combinators
-- ---------------------------------------------------------------------------

tryS :: ParserSyntax f s a -> ParserSyntax f s a
tryS (ParserSyntax p) = ParserSyntax $ Op $ R $ R $ CombTry p

optionalS :: (Uncons f s) => ParserSyntax f s a -> ParserSyntax f s (Maybe a)
optionalS = optional

manyS :: ParserSyntax f s a -> ParserSyntax f s [a]
manyS (ParserSyntax p) = ParserSyntax $ Op $ R $ R $ CombMany p

someS :: (Uncons f s) => ParserSyntax f s a -> ParserSyntax f s [a]
someS p = (:) <$> p <*> manyS p

skipManyS :: (Uncons f s) => ParserSyntax f s a -> ParserSyntax f s ()
skipManyS p = void (manyS p)

countS :: (Uncons f s) => Int -> ParserSyntax f s a -> ParserSyntax f s [a]
countS n p
  | n <= 0 = pure []
  | otherwise = (:) <$> p <*> countS (n - 1) p

sepByS :: (Uncons f s) => ParserSyntax f s a -> ParserSyntax f s b -> ParserSyntax f s [a]
sepByS p sep = sepBy1S p sep <|> pure []

sepBy1S :: (Uncons f s) => ParserSyntax f s a -> ParserSyntax f s b -> ParserSyntax f s [a]
sepBy1S p sep = p >>= \x -> manyS (tryS (sep *> p)) >>= \xs -> pure (x : xs)

withOptionS ::
  (Uncons f s) =>
  ParserSyntax f s a ->
  (a -> ParserSyntax f s b) ->
  ParserSyntax f s b ->
  ParserSyntax f s b
withOptionS p f def = (p >>= f) <|> def

-- ---------------------------------------------------------------------------
-- Static analysis
-- ---------------------------------------------------------------------------

-- | Possible first tokens of a parser, plus whether it can succeed without
-- consuming input.
data FirstSet s = FirstSet
  { nullable :: Bool,
    firstKind :: FirstKind s
  }

-- | Classification of a first-set.
data FirstKind s
  = FSNone
  | FSPred (s -> Bool)
  | FSUniversal

instance Show (FirstSet s) where
  show fs =
    "FirstSet {nullable = "
      ++ show (nullable fs)
      ++ ", firstKind = "
      ++ showKind (firstKind fs)
      ++ "}"
    where
      showKind FSNone = "FSNone"
      showKind (FSPred _) = "FSPred <function>"
      showKind FSUniversal = "FSUniversal"

-- | Nullable first-set.
nullFS :: FirstSet s
nullFS = FirstSet True FSNone

-- | First-set for a single token predicate.
predFS :: (s -> Bool) -> FirstSet s
predFS p = FirstSet False (FSPred p)

-- | Universal first-set.
universalFS :: FirstSet s
universalFS = FirstSet False FSUniversal

-- | Union of two first-sets.
unionFS :: FirstSet s -> FirstSet s -> FirstSet s
unionFS x y =
  FirstSet
    { nullable = nullable x || nullable y,
      firstKind = case (firstKind x, firstKind y) of
        (FSUniversal, _) -> FSUniversal
        (_, FSUniversal) -> FSUniversal
        (FSNone, k) -> k
        (k, FSNone) -> k
        (FSPred px, FSPred py) -> FSPred (\c -> px c || py c)
    }

-- | Sequential composition of first-sets.
seqFS :: FirstSet s -> FirstSet s -> FirstSet s
seqFS x y =
  FirstSet
    { nullable = nullable x && nullable y,
      firstKind = case (nullable x, firstKind x, firstKind y) of
        (True, FSNone, k) -> k
        (True, k, FSNone) -> k
        (True, FSPred px, FSPred py) -> FSPred (\c -> px c || py c)
        (True, FSUniversal, _) -> FSUniversal
        (True, _, FSUniversal) -> FSUniversal
        (False, k, _) -> k
    }

-- | Compute the first-set of an arbitrary syntax subtree.
firstSetSyntax :: Syntax (ParserSyntaxSig f s) (SArr f) x y -> FirstSet s
firstSetSyntax (Lift _) = nullFS
firstSetSyntax (Op op) = case op of
  L (SigCompose g f) -> firstSetSyntax f `seqFS` firstSetSyntax g
  R (L prim) -> case prim of
    PrimNext -> universalFS
    PrimSatisfy p -> predFS p
    PrimChar c -> predFS (== c)
    PrimString [] -> nullFS
    PrimString (c : _) -> predFS (== c)
    PrimEndOfInput -> nullFS
    PrimTakeRest -> universalFS
  R (R comb) -> case comb of
    CombAp pf pa -> firstSetSyntax pf `seqFS` firstSetSyntax pa
    CombBind _ _ -> nullFS
    CombAlt p1 p2 -> firstSetSyntax p1 `unionFS` firstSetSyntax p2
    CombMany p -> FirstSet True (firstKind (firstSetSyntax p))
    CombTry p -> firstSetSyntax p
    CombFmap _ p -> firstSetSyntax p

-- | Compute the first-set of a parser syntax tree.
firstSet :: ParserSyntax f s a -> FirstSet s
firstSet = firstSetSyntax . unParserSyntax

-- | Detect unreachable branches in choice nodes. A branch is unreachable when
-- the left side can consume any token that the right side can consume.
unreachableBranches :: ParserSyntax f s a -> [String]
unreachableBranches = go [] . unParserSyntax
  where
    go :: [FirstSet s] -> Syntax (ParserSyntaxSig f s) (SArr f) x y -> [String]
    go _ (Lift _) = []
    go acc (Op op) = case op of
      L (SigCompose g f) ->
        go acc f ++ go (acc ++ [firstSetSyntax f]) g
      R (L _) -> []
      R (R comb) -> case comb of
        CombAlt p1 p2 ->
          let left = firstSetSyntax p1
              rights = acc ++ [left]
           in check left (firstSetSyntax p2)
                ++ go rights p1
                ++ go rights p2
        CombAp pf pa -> go acc pf ++ go acc pa
        CombBind _ _ -> []
        CombMany p -> go acc p
        CombTry p -> go acc p
        CombFmap _ p -> go acc p

    check :: FirstSet s -> FirstSet s -> [String]
    check left right =
      case (firstKind left, firstKind right) of
        (FSUniversal, FSPred _) ->
          ["right branch can never fire (left consumes any token)"]
        (FSUniversal, FSUniversal) ->
          ["right branch can never fire (left consumes any token)"]
        _ -> []

-- ---------------------------------------------------------------------------
-- Regex extraction
-- ---------------------------------------------------------------------------

-- | A simple regular-expression AST.
data Regex s
  = REEmpty
  | REAny
  | REChar s
  | REString [s]
  | REClass (s -> Bool)
  | REAlt (Regex s) (Regex s)
  | RESeq (Regex s) (Regex s)
  | REStar (Regex s)

instance (Show s) => Show (Regex s) where
  show REEmpty = "REEmpty"
  show REAny = "REAny"
  show (REChar c) = "REChar " ++ show c
  show (REString cs) = "REString " ++ show cs
  show (REClass _) = "REClass <function>"
  show (REAlt r1 r2) = "REAlt (" ++ show r1 ++ ") (" ++ show r2 ++ ")"
  show (RESeq r1 r2) = "RESeq (" ++ show r1 ++ ") (" ++ show r2 ++ ")"
  show (REStar r) = "REStar (" ++ show r ++ ")"

-- | Extract a regex from a syntax tree, if it is regular. Returns 'Nothing' for
-- primitives or combinators that cannot be expressed as regular expressions.
toRegex :: ParserSyntax f s a -> Maybe (Regex s)
toRegex = go . unParserSyntax
  where
    go :: Syntax (ParserSyntaxSig f s) (SArr f) x y -> Maybe (Regex s)
    go (Lift _) = Just REEmpty
    go (Op op) = case op of
      L (SigCompose g f) -> RESeq <$> go f <*> go g
      R (L prim) -> case prim of
        PrimNext -> Just REAny
        PrimSatisfy p -> Just (REClass p)
        PrimChar c -> Just (REChar c)
        PrimString cs -> Just (REString cs)
        PrimEndOfInput -> Just REEmpty
        PrimTakeRest -> Nothing
      R (R comb) -> case comb of
        CombAp pf pa -> RESeq <$> go pf <*> go pa
        CombBind _ _ -> Nothing
        CombAlt p1 p2 -> REAlt <$> go p1 <*> go p2
        CombMany p -> REStar <$> go p
        CombTry p -> go p
        CombFmap _ p -> go p

-- ---------------------------------------------------------------------------
-- Brzozowski derivatives
-- ---------------------------------------------------------------------------

-- | The parser that remains after consuming one token.
--
-- This is the core of the coalgebraic compiler: a 'Process' machine state is a
-- parser syntax tree, and consuming a token transitions to its derivative.
-- The implementation covers the applicative + alternative + 'many' fragment;
-- 'CombBind' is rejected because it is dependent composition.
derive :: (Eq s, Uncons f s) => s -> ParserSyntax f s a -> ParserSyntax f s a
derive c (ParserSyntax syn) = ParserSyntax (deriveSyntax c syn)

-- | Syntax-level derivative.
deriveSyntax ::
  forall s f a.
  (Eq s, Uncons f s) =>
  s ->
  Syntax (ParserSyntaxSig f s) (SArr f) () (These a f) ->
  Syntax (ParserSyntaxSig f s) (SArr f) () (These a f)
deriveSyntax c syn = case syn of
  Lift _ -> emptyResultSyntax
  Op op -> case op of
    L (SigCompose _ _) ->
      error "deriveSyntax: explicit SigCompose not supported; use fmap/Applicative/Alternative constructors"
    R (L prim) -> derivePrim c prim
    R (R comb) -> deriveComb c comb

-- | Derivative of a primitive.
derivePrim ::
  forall s f a.
  (Eq s) =>
  s ->
  SigPrim f s (SArr f) (Syntax (ParserSyntaxSig f s) (SArr f)) () (These a f) ->
  Syntax (ParserSyntaxSig f s) (SArr f) () (These a f)
derivePrim c prim = case prim of
  PrimNext -> pureSyntax c
  PrimSatisfy p -> if p c then pureSyntax c else emptyResultSyntax
  PrimChar d -> if c == d then pureSyntax c else emptyResultSyntax
  PrimString [] -> emptyResultSyntax
  PrimString (d : ds) ->
    if c == d
      then fmapSyntax (d :) (stringSyntax ds)
      else emptyResultSyntax
  PrimEndOfInput -> emptyResultSyntax
  PrimTakeRest -> emptyResultSyntax

-- | Derivative of a combinator.
deriveComb ::
  forall s f a.
  (Eq s, Uncons f s) =>
  s ->
  SigComb f s (SArr f) (Syntax (ParserSyntaxSig f s) (SArr f)) () (These a f) ->
  Syntax (ParserSyntaxSig f s) (SArr f) () (These a f)
deriveComb c comb = case comb of
  CombAp pf pa ->
    let left = Op (R (R (CombAp (deriveSyntax c pf) pa)))
        right = case nullableValue (ParserSyntax pf) of
          Just g -> fmapSyntax g (deriveSyntax c pa)
          Nothing -> emptyResultSyntax
     in Op (R (R (CombAlt left right)))
  CombBind _ _ -> error "deriveComb: CombBind not supported"
  CombAlt p1 p2 ->
    Op (R (R (CombAlt (deriveSyntax c p1) (deriveSyntax c p2))))
  CombMany p ->
    Op
      ( R
          ( R
              ( CombAp
                  (fmapSyntax (:) (deriveSyntax c p))
                  (Op (R (R (CombMany p))))
              )
          )
      )
  CombTry p -> deriveSyntax c p
  CombFmap g p -> fmapSyntax g (deriveSyntax c p)

-- | Syntax tree that always fails, returning the input stream unchanged.
emptyResultSyntax :: Syntax (ParserSyntaxSig f s) (SArr f) () (These a f)
emptyResultSyntax = Lift (SArr (\(st, ()) -> (st, That st)))

-- | Syntax tree that returns a constant value without consuming input.
pureSyntax :: a -> Syntax (ParserSyntaxSig f s) (SArr f) () (These a f)
pureSyntax a = Lift (SArr (\(st, ()) -> (st, These a st)))

-- | Lift a pure function over the result of a syntax tree using the structural
-- 'CombFmap' node.
fmapSyntax ::
  (a -> b) ->
  Syntax (ParserSyntaxSig f s) (SArr f) () (These a f) ->
  Syntax (ParserSyntaxSig f s) (SArr f) () (These b f)
fmapSyntax g syn = Op (R (R (CombFmap g syn)))

-- | Syntax tree for matching a fixed string.
stringSyntax ::
  (Eq s) => [s] -> Syntax (ParserSyntaxSig f s) (SArr f) () (These [s] f)
stringSyntax cs = Op (R (L (PrimString cs)))

-- | Check whether a parser can succeed without consuming any input, and if
-- so extract the value it would return.
nullableValue :: forall f s a. (Uncons f s) => ParserSyntax f s a -> Maybe a
nullableValue p
  | nullable (firstSet p) =
      case runParserSyntaxIdentity p (nil @f @s) of
        These a _ -> Just a
        _ -> Nothing
  | otherwise = Nothing
