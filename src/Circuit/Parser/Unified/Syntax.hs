{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Reifiable parser syntax.
--
-- The parser type in "Circuit.Parser.Unified" hides primitive operations inside
-- @Kleisli m@ closures. This module exposes those primitives as constructors in
-- a syntax tree, so the same parser can be executed /and/ analyzed.
--
-- The plumbing — sequential composition and backtracking choice — is the free
-- @AlgLoop Either@ from "Circuit.Algebra". Primitives and a small set of
-- structural combinators ('Ap', 'Bind', 'Alt', 'Many') live as additional
-- signatures. Executing a syntax tree is an algebra fold into
-- @Loop Either (Kleisli m)@; static analysis is a fold into other targets.
--
-- === doctests
--
-- >>> import Data.Functor.Identity (Identity)
-- >>> import Data.These (These(..))
-- >>> import Control.Applicative ((<|>))
-- >>> import Circuit.Parser.Unified.Syntax (charS, stringS, manyS, firstSet, toRegex)
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
module Circuit.Parser.Unified.Syntax
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
  )
where

import Circuit qualified as C
import Circuit.Algebra
  ( Algebra (..),
    (:+:) (..),
    SigCompose (..),
    SigKnot (..),
    Syntax (..),
    evalInto,
  )
import Circuit.Category (Category (..), (.>))
import Circuit.Channel (Traced)
import Circuit.Parser.Unified
  ( Parser (..),
    Uncons (..),
  )
import Circuit.Parser.Unified qualified as PU
import Control.Applicative (Alternative (empty, (<|>)))
import Control.Arrow (Kleisli (..))
import Control.Monad (MonadPlus, void)
import Data.Bifunctor (first)
import Data.Functor.Identity (Identity (..))
import Data.These (These (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import Prelude hiding (id, (.))

-- $setup
-- >>> import Data.Functor.Identity (Identity)
-- >>> import Data.These (These (..))
-- >>> import Control.Applicative ((<|>))
-- >>> import Circuit.Parser.Unified.Syntax (charS, stringS, manyS, firstSet, toRegex)

-- ---------------------------------------------------------------------------
-- Signatures
-- ---------------------------------------------------------------------------

-- | One-step parser primitives. Each constructor names a leaf operation on a
-- stream of elements @s@ with stream type @f@.
data SigPrim (f :: Type) (s :: Type) (arr :: Type -> Type -> Type) (rec :: Type -> Type -> Type) (a :: Type) (b :: Type) where
  PrimNext :: SigPrim f s arr rec f (These s f)
  PrimSatisfy :: (s -> Bool) -> SigPrim f s arr rec f (These s f)
  PrimChar :: Eq s => s -> SigPrim f s arr rec f (These s f)
  PrimString :: Eq s => [s] -> SigPrim f s arr rec f (These [s] f)
  PrimEndOfInput :: SigPrim f s arr rec f (These () f)
  PrimTakeRest :: SigPrim f s arr rec f (These f f)

-- | Structural combinators that cannot be expressed as pure @AlgLoop Either@
-- plumbing while preserving the syntax tree. These are eliminated by the
-- execution algebra.
data SigComb (f :: Type) (s :: Type) (arr :: Type -> Type -> Type) (rec :: Type -> Type -> Type) (a :: Type) (b :: Type) where
  CombAp :: rec f (These (a -> b) f) -> rec f (These a f) -> SigComb f s arr rec f (These b f)
  CombBind :: rec f (These a f) -> (a -> rec f (These b f)) -> SigComb f s arr rec f (These b f)
  CombAlt :: rec f (These a f) -> rec f (These a f) -> SigComb f s arr rec f (These a f)
  CombMany :: rec f (These a f) -> SigComb f s arr rec f (These [a] f)
  CombTry :: rec f (These a f) -> SigComb f s arr rec f (These a f)

-- | The full parser signature: composition, traced choice, primitives, and
-- structural combinators.
type ParserSyntaxSig f s = SigCompose :+: SigKnot Either :+: SigPrim f s :+: SigComb f s

-- | A parser syntax tree with stream type @f@, element type @s@, and result
-- type @a@.
newtype ParserSyntax (f :: Type) (s :: Type) (a :: Type) = ParserSyntax
  { unParserSyntax ::
      Syntax (ParserSyntaxSig f s) (Kleisli Identity) f (These a f)
  }

-- | The underlying syntax type supports sequential composition via 'SigCompose'.
-- This is the category that 'AlgLoop Either' would have provided; we provide it
-- manually because the signature also carries primitive and combinator nodes.
instance Category (Syntax (ParserSyntaxSig f s) (Kleisli Identity)) where
  id = Lift id
  f . g = Op (L (SigCompose f g))

-- ---------------------------------------------------------------------------
-- Algebra for execution
-- ---------------------------------------------------------------------------

-- | Map primitive operations to their implementations in @Loop Either
-- (Kleisli m)@.
instance
  (Monad m, Uncons f s, Traced Either (Kleisli m)) =>
  Algebra (SigPrim f s) (Kleisli Identity) (C.Loop Either (Kleisli m))
  where
  type
    Ctx (SigPrim f s) (Kleisli Identity) (C.Loop Either (Kleisli m)) =
      (Monad m, Uncons f s, Traced Either (Kleisli m))
  alg _ _ PrimNext = unParser (PU.next @m @f @s)
  alg _ _ (PrimSatisfy p) = unParser (PU.satisfy @m @f @s p)
  alg _ _ (PrimChar c) = unParser (PU.char @m @f @s c)
  alg _ _ (PrimString cs) = unParser (PU.string @m @f @s cs)
  alg _ _ PrimEndOfInput = unParser (PU.endOfInput @m @f @s)
  alg _ _ PrimTakeRest = unParser (PU.takeRest @m @f @s)

-- Helpers that live in the concrete 'Loop' world. They take a 'Proxy s' because
-- the element type is phantom in the loop representation.
apP ::
  forall m f s a b.
  (Monad m, Uncons f s, Traced Either (Kleisli m)) =>
  Proxy s ->
  C.Loop Either (Kleisli m) f (These (a -> b) f) ->
  C.Loop Either (Kleisli m) f (These a f) ->
  C.Loop Either (Kleisli m) f (These b f)
apP _ pf pa = C.Lift $ Kleisli $ \s -> do
  let app g s' = runKleisli (C.run pa) s' >>= \case
        That _ -> pure (That s)
        res -> pure (first g res)
  runKleisli (C.run pf) s >>= \case
    That _ -> pure (That s)
    This g -> app g (nil @f @s)
    These g s' -> app g s'

altP ::
  forall m f a.
  (Monad m, Traced Either (Kleisli m)) =>
  C.Loop Either (Kleisli m) f (These a f) ->
  C.Loop Either (Kleisli m) f (These a f) ->
  C.Loop Either (Kleisli m) f (These a f)
altP p1 p2 = C.trace $ C.Lift $ Kleisli $ \case
  Right s -> do
    res <- runKleisli (C.run p1) s
    case res of
      That s' -> pure (Left s')
      _ -> pure (Right res)
  Left s -> do
    res <- runKleisli (C.run p2) s
    pure (Right res)

manyP ::
  forall m f s a.
  (Monad m, Uncons f s, Traced Either (Kleisli m)) =>
  Proxy s ->
  C.Loop Either (Kleisli m) f (These a f) ->
  C.Loop Either (Kleisli m) f (These [a] f)
manyP pxy p = someP pxy p `altP` (C.Lift $ Kleisli $ \s -> pure (These [] s))

someP ::
  forall m f s a.
  (Monad m, Uncons f s, Traced Either (Kleisli m)) =>
  Proxy s ->
  C.Loop Either (Kleisli m) f (These a f) ->
  C.Loop Either (Kleisli m) f (These [a] f)
someP pxy p = apP pxy (fmapP (:) p) (manyP pxy p)
  where
    fmapP :: (a -> b) -> C.Loop Either (Kleisli m) f (These a f) -> C.Loop Either (Kleisli m) f (These b f)
    fmapP g p' = p' .> C.Lift (Kleisli (pure . first g))

tryP ::
  forall m f a.
  (Monad m, Traced Either (Kleisli m)) =>
  C.Loop Either (Kleisli m) f (These a f) ->
  C.Loop Either (Kleisli m) f (These a f)
tryP p = C.Lift $ Kleisli $ \s -> do
  res <- runKleisli (C.run p) s
  pure $ case res of
    That _ -> That s
    result -> result

-- | Interpret syntax into a concrete parser.
runParserSyntax ::
  (Monad m, Uncons f s, Traced Either (Kleisli m)) =>
  ParserSyntax f s a ->
  Parser m f s a
runParserSyntax (ParserSyntax syn) = Parser $ evalInto emb syn
  where
    emb (Kleisli g) = C.Lift $ Kleisli $ \x -> pure (runIdentity (g x))

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

instance Uncons f s => Functor (ParserSyntax f s) where
  fmap g (ParserSyntax p) =
    ParserSyntax (p .> Lift (Kleisli (pure . first g)))

instance Uncons f s => Applicative (ParserSyntax f s) where
  pure a = ParserSyntax $ Lift $ Kleisli $ \x -> pure (These a x)
  ParserSyntax pf <*> ParserSyntax pa =
    ParserSyntax $ Op $ R $ R $ R $ CombAp pf pa

instance (Uncons f s) => Monad (ParserSyntax f s) where
  ParserSyntax m >>= k =
    ParserSyntax $
      Op $
        R $
          R $
            R $
              CombBind m (unParserSyntax . k)

instance
  (Monad m, Uncons f s, Traced Either (Kleisli m)) =>
  Algebra (SigComb f s) (Kleisli Identity) (C.Loop Either (Kleisli m))
  where
  alg _ rec (CombAp pf pa) = apP (Proxy @s) (rec pf) (rec pa)
  alg _ rec (CombBind p k) = bindP (Proxy @s) (rec p) (\a -> rec (k a))
  alg _ rec (CombAlt p1 p2) = altP (rec p1) (rec p2)
  alg _ rec (CombMany p) = manyP (Proxy @s) (rec p)
  alg _ rec (CombTry p) = tryP (rec p)

bindP ::
  forall m f s a b.
  (Monad m, Uncons f s, Traced Either (Kleisli m)) =>
  Proxy s ->
  C.Loop Either (Kleisli m) f (These a f) ->
  (a -> C.Loop Either (Kleisli m) f (These b f)) ->
  C.Loop Either (Kleisli m) f (These b f)
bindP _ m k = C.Lift $ Kleisli $ \s -> do
  runKleisli (C.run m) s >>= \case
    That s' -> pure (That s')
    This a -> runKleisli (C.run (k a)) (nil @f @s)
    These a s' -> runKleisli (C.run (k a)) s'

instance (Uncons f s) => Alternative (ParserSyntax f s) where
  empty = ParserSyntax $ Lift $ Kleisli $ \x -> pure (That x)
  ParserSyntax p1 <|> ParserSyntax p2 =
    ParserSyntax $ Op $ R $ R $ R $ CombAlt p1 p2

instance (Uncons f s) => MonadPlus (ParserSyntax f s)

-- ---------------------------------------------------------------------------
-- Primitive constructors
-- ---------------------------------------------------------------------------

nextS :: ParserSyntax f s s
nextS = ParserSyntax $ Op $ R $ R $ L PrimNext

anyTokenS :: ParserSyntax f s s
anyTokenS = nextS

satisfyS :: (s -> Bool) -> ParserSyntax f s s
satisfyS p = ParserSyntax $ Op $ R $ R $ L (PrimSatisfy p)

charS :: Eq s => s -> ParserSyntax f s s
charS c = ParserSyntax $ Op $ R $ R $ L (PrimChar c)

stringS :: Eq s => [s] -> ParserSyntax f s [s]
stringS cs = ParserSyntax $ Op $ R $ R $ L (PrimString cs)

endOfInputS :: ParserSyntax f s ()
endOfInputS = ParserSyntax $ Op $ R $ R $ L PrimEndOfInput

takeRestS :: ParserSyntax f s f
takeRestS = ParserSyntax $ Op $ R $ R $ L PrimTakeRest

-- ---------------------------------------------------------------------------
-- Combinators
-- ---------------------------------------------------------------------------

tryS :: ParserSyntax f s a -> ParserSyntax f s a
tryS (ParserSyntax p) = ParserSyntax $ Op $ R $ R $ R $ CombTry p

optionalS :: (Uncons f s) => ParserSyntax f s a -> ParserSyntax f s (Maybe a)
optionalS p = (Just <$> p) <|> pure Nothing

manyS :: ParserSyntax f s a -> ParserSyntax f s [a]
manyS (ParserSyntax p) = ParserSyntax $ Op $ R $ R $ R $ CombMany p

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
firstSetSyntax :: Syntax (ParserSyntaxSig f s) (Kleisli Identity) x y -> FirstSet s
firstSetSyntax (Lift _) = nullFS
firstSetSyntax (Op op) = case op of
  L (SigCompose g f) -> firstSetSyntax f `seqFS` firstSetSyntax g
  R (L (SigKnot body)) -> firstSetSyntax body
  R (R (L prim)) -> case prim of
    PrimNext -> universalFS
    PrimSatisfy p -> predFS p
    PrimChar c -> predFS (== c)
    PrimString [] -> nullFS
    PrimString (c : _) -> predFS (== c)
    PrimEndOfInput -> nullFS
    PrimTakeRest -> universalFS
  R (R (R comb)) -> case comb of
    CombAp pf pa -> firstSetSyntax pf `seqFS` firstSetSyntax pa
    CombBind _ _ -> nullFS
    CombAlt p1 p2 -> firstSetSyntax p1 `unionFS` firstSetSyntax p2
    CombMany p -> FirstSet True (firstKind (firstSetSyntax p))
    CombTry p -> firstSetSyntax p

-- | Compute the first-set of a parser syntax tree.
firstSet :: ParserSyntax f s a -> FirstSet s
firstSet = firstSetSyntax . unParserSyntax

-- | Detect unreachable branches in choice nodes. A branch is unreachable when
-- the left side can consume any token that the right side can consume.
unreachableBranches :: ParserSyntax f s a -> [String]
unreachableBranches = go [] . unParserSyntax
  where
    go :: [FirstSet s] -> Syntax (ParserSyntaxSig f s) (Kleisli Identity) x y -> [String]
    go _ (Lift _) = []
    go acc (Op op) = case op of
      L (SigCompose g f) ->
        go acc f ++ go (acc ++ [firstSetSyntax f]) g
      R (L (SigKnot body)) -> go acc body
      R (R (L _)) -> []
      R (R (R comb)) -> case comb of
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

instance Show s => Show (Regex s) where
  show REEmpty = "REEmpty"
  show REAny = "REAny"
  show (REChar c) = "REChar " ++ show c
  show (REString cs) = "REString " ++ show cs
  show (REClass _) = "REClass <function>"
  show (REAlt r1 r2) = "REAlt (" ++ show r1 ++ ") (" ++ show r2 ++ ")"
  show (RESeq r1 r2) = "RESeq (" ++ show r1 ++ ") (" ++ show r2 ++ ")"
  show (REStar r) = "REStar (" ++ show r ++ ")"

-- | Extract a regex from a syntax tree, if it is regular. Returns 'Nothing' for
-- primitives or knot patterns that cannot be expressed as regular expressions.
toRegex :: ParserSyntax f s a -> Maybe (Regex s)
toRegex = go . unParserSyntax
  where
    go :: Syntax (ParserSyntaxSig f s) (Kleisli Identity) x y -> Maybe (Regex s)
    go (Lift _) = Just REEmpty
    go (Op op) = case op of
      L (SigCompose g f) -> RESeq <$> go f <*> go g
      R (L (SigKnot body)) -> REStar <$> go body
      R (R (L prim)) -> case prim of
        PrimNext -> Just REAny
        PrimSatisfy p -> Just (REClass p)
        PrimChar c -> Just (REChar c)
        PrimString cs -> Just (REString cs)
        PrimEndOfInput -> Just REEmpty
        PrimTakeRest -> Nothing
      R (R (R comb)) -> case comb of
        CombAp pf pa -> RESeq <$> go pf <*> go pa
        CombBind _ _ -> Nothing
        CombAlt p1 p2 -> REAlt <$> go p1 <*> go p2
        CombMany p -> REStar <$> go p
        CombTry p -> go p
