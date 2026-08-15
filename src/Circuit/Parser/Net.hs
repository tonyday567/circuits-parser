{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Experimental parser syntax built over @AlgNet Either@ plus the two
-- universal @Either@ operations that the free traced PROP omits.
--
-- * 'SigEitherFunctor' — the functorial action @bimap id h@ on @Either f@.
--   This is exactly what applicative sequencing needs to apply the second
--   parser only on the success branch of the first.
--
-- * 'SigEitherCase' — the copair @[f, g]@, i.e. case analysis on @Either@.
--   Traced choice ('<|>') needs this to route the initial stream to the first
--   parser and the failure-feedback stream to the second.
--
-- With these two additions, '<*>' and '<|>' become structural over generic
-- categorical nodes.  '>>=' remains parser-specific (it is a dependent
-- composition: the right-hand parser is chosen by a runtime value), so it is
-- admitted as a 'SigBind' node, just as "Circuit.Parser.Syntax" admits
-- 'CombBind'.
--
-- The result type is @Either f (a, f)@: 'Left' for failure with the original
-- stream, 'Right' for success with value and leftover stream.
--
-- === Comparison with "Circuit.Parser.Syntax"
--
-- "Circuit.Parser.Syntax" keeps parser-specific structure explicit in
-- a small set of opaque combinators ('CombAp', 'CombBind', 'CombAlt',
-- 'CombMany', 'CombTry').  That design is pragmatic: static analysis such as
-- 'firstSet', 'unreachableBranches' and 'toRegex' can pattern-match directly
-- on those constructors and ignore the underlying plumbing.  The cost is that
-- '<*>', '<|>' and 'many' are admitted as non-generic nodes; they are not
-- made of the same categorical parts as the rest of the algebra.
--
-- This module goes the other way.  It keeps the underlying @AlgNet Either@
-- plumbing — composition, @par@, @swap@, copy/discard, and traced feedback —
-- and adds only the two pieces of the @Either f@ coproduct structure that the
-- free traced PROP forgets: the functorial action on the right summand
-- ('SigEitherFunctor') and the copair / case analysis ('SigEitherCase').  With
-- those two additions '<*>' and '<|>' are built from generic nodes; only
-- '>>=' remains parser-specific ('SigBind').
--
-- The obstruction that 'CombAp' solves in "Circuit.Parser.Syntax" is
-- the same one that 'SigEitherFunctor' solves here.  In @Loop Either (Kleisli
-- m)@ a parser is a morphism @f -> Either f (a, f)@.  The category gives us
-- sequential composition and the trace gives us backtracking choice, but it
-- does not provide the /interchange/ map that runs a second process inside the
-- success branch of a first while preserving both the intermediate value and
-- the intermediate stream.  'EitherMap' is exactly that map: it is the
-- strength @bimap id h@ of the @Either f@ tensor, or equivalently the right
-- injection of a coproduct functor.  'EitherCase' is the dual piece: the
-- copair that lets us merge two success-typed branches after the trace has
-- routed the stream through one parser or the other.
--
-- Why this matters: because '<*>' is now structural, an analysis target can
-- choose to understand it as generic wiring, or it can still recognise the
-- recurring 'copyN', 'parN', 'swapN', 'mapRightN', 'eitherCaseN' pattern if it
-- wants parser-level facts.  The algebra is more uniform, but the static
-- analyses are harder to write: they must interpret the wiring rather than
-- reading a single constructor.  The "Syntax" approach trades a little
-- algebraic purity for direct inspectability; the "Net" approach pays that
-- price to push more of the parser vocabulary into the shared algebra.
--
-- Implementation note: 'SigBimonoid' used to bundle copy/discard with
-- plus/zero, and parser plumbing only ever needs copy/discard.  The upstream
-- split into 'SigCopyDiscard' / 'SigMergeZero' means this module now carries
-- only the 'Dg.Copy' and 'Dg.Discard' instances for @Kleisli Identity@; the degenerate
-- 'MergeZero' orphan is gone.
module Circuit.Parser.Net
  ( -- * Extra signatures
    SigEitherFunctor (..),
    SigEitherCase (..),
    SigBind (..),

    -- * Syntax tree
    ParserNet (..),

    -- * Primitive constructors
    nextN,
    anyTokenN,
    satisfyN,
    charN,
    stringN,
    endOfInputN,
    takeRestN,

    -- * Repetition
    manyN,
    someN,

    -- * Execution
    runParserNet,
    runParserNetIdentity,
  )
where

import Circuit qualified as C
import Circuit.Algebra
  ( Algebra (..),
    SigCompose (..),
    SigCopyDiscard (..),
    SigKnot (..),
    SigMergeZero (..),
    SigPar (..),
    SigSwap (..),
    Syntax (..),
    evalInto,
    (:+:) (..),
  )
import Circuit.Category (Category (..), (.>))
import Circuit.Channel (Traced, strengthD)
import Circuit.Dagger qualified as Dg
import Circuit.Parser
  ( Parser (..),
    Uncons (..),
  )
import Circuit.Parser qualified as PU
import Circuit.Tensor ()
import Control.Applicative (Alternative (empty, (<|>)))
import Control.Arrow (Kleisli (..))
import Control.Monad (MonadPlus)
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.These (These (..))
import Prelude hiding (id, (.))

-- $setup
-- >>> import Data.Functor.Identity (Identity)
-- >>> import Data.These (These (..))
-- >>> import Control.Applicative ((<|>))
-- >>> import Circuit.Parser.Net (charN, stringN, manyN, runParserNetIdentity)

-- ---------------------------------------------------------------------------
-- Signatures missing from AlgNet Either
-- ---------------------------------------------------------------------------

-- | Functorial action of @Either f@: apply a sub-morphism inside the 'Right'
-- branch, leaving a 'Left' failure untouched.
--
-- This is exactly @bimap id h@, or 'strength' for the @Either@ tensor.  It is
-- the missing piece for applicative sequencing: after the first parser
-- succeeds we must run the second parser /only/ on that success branch.
data SigEitherFunctor (f :: Type) (arr :: Type -> Type -> Type) (rec :: Type -> Type -> Type) (a :: Type) (b :: Type) where
  EitherMap :: rec x y -> SigEitherFunctor f arr rec (Either f x) (Either f y)

-- | Coproduct copair: case analysis on @Either@.  Given morphisms for the
-- 'Left' and 'Right' branches, build a morphism out of the sum.
--
-- Traced choice ('<|>') needs this to dispatch the initial input to the first
-- parser and the failure-feedback input to the second parser.  The free
-- traced PROP over @Either@ does not include this as a generator because it
-- assumes only the tensor structure, not the full coproduct universal
-- property.
data SigEitherCase (arr :: Type -> Type -> Type) (rec :: Type -> Type -> Type) (a :: Type) (b :: Type) where
  EitherCase :: rec x b -> rec y b -> SigEitherCase arr rec (Either x y) b

-- | Dependent sequential composition.  The right-hand parser is produced by a
-- Haskell function from the value returned by the left-hand parser, so it
-- cannot be expressed from generic categorical nodes alone.
data SigBind (f :: Type) (s :: Type) (arr :: Type -> Type -> Type) (rec :: Type -> Type -> Type) (a :: Type) (b :: Type) where
  Bind :: rec f (Either f (a, f)) -> (a -> rec f (Either f (b, f))) -> SigBind f s arr rec f (Either f (b, f))

-- ---------------------------------------------------------------------------
-- Parser syntax
-- ---------------------------------------------------------------------------

-- | Signature for the structural parser net: the full @AlgNet Either@
-- machinery plus the two universal @Either@ operations and a bind node for
-- the monadic layer.
type ParserNetSig f s =
  SigCompose :+: SigKnot Either :+: SigPar :+: SigSwap :+: SigCopyDiscard :+: SigMergeZero :+: SigEitherFunctor f :+: SigEitherCase :+: SigBind f s

-- | Parser syntax over the extended @AlgNet Either@-style signature with
-- result type @Either f (a, f)@.
newtype ParserNet (f :: Type) (s :: Type) (a :: Type) = ParserNet
  { unParserNet :: Syntax (ParserNetSig f s) (Kleisli Identity) f (Either f (a, f))
  }

-- ---------------------------------------------------------------------------
-- Shallow constructors for signature nodes
-- ---------------------------------------------------------------------------

liftK :: (a -> b) -> Syntax (ParserNetSig f s) (Kleisli Identity) a b
liftK g = Lift (Kleisli (pure . g))

composeN ::
  Syntax (ParserNetSig f s) (Kleisli Identity) b c ->
  Syntax (ParserNetSig f s) (Kleisli Identity) a b ->
  Syntax (ParserNetSig f s) (Kleisli Identity) a c
composeN g f = Op (L (SigCompose g f))

-- | Forward composition for parser-net syntax.
--
-- @f .>> g@ runs @f@ then @g@.
infixl 1 .>>

(.>>) ::
  Syntax (ParserNetSig f s) (Kleisli Identity) a b ->
  Syntax (ParserNetSig f s) (Kleisli Identity) b c ->
  Syntax (ParserNetSig f s) (Kleisli Identity) a c
(.>>) f g = composeN g f

knotN ::
  Syntax (ParserNetSig f s) (Kleisli Identity) (Either c a) (Either c b) ->
  Syntax (ParserNetSig f s) (Kleisli Identity) a b
knotN body = Op (R (L (SigKnot body)))

parN ::
  Syntax (ParserNetSig f s) (Kleisli Identity) a b ->
  Syntax (ParserNetSig f s) (Kleisli Identity) c d ->
  Syntax (ParserNetSig f s) (Kleisli Identity) (a, c) (b, d)
parN f g = Op (R (R (L (SigPar f g))))

swapN :: Syntax (ParserNetSig f s) (Kleisli Identity) (a, b) (b, a)
swapN = Op (R (R (R (L SigSwap))))

copyN :: Syntax (ParserNetSig f s) (Kleisli Identity) a (a, a)
copyN = Op (R (R (R (R (L SigCopy)))))

mapRightN ::
  Syntax (ParserNetSig f s) (Kleisli Identity) a b ->
  Syntax (ParserNetSig f s) (Kleisli Identity) (Either f a) (Either f b)
mapRightN g = Op (R (R (R (R (R (R (L (EitherMap g))))))))

eitherCaseN ::
  Syntax (ParserNetSig f s) (Kleisli Identity) x b ->
  Syntax (ParserNetSig f s) (Kleisli Identity) y b ->
  Syntax (ParserNetSig f s) (Kleisli Identity) (Either x y) b
eitherCaseN f g = Op (R (R (R (R (R (R (R (L (EitherCase f g)))))))))

bindN ::
  Syntax (ParserNetSig f s) (Kleisli Identity) f (Either f (a, f)) ->
  (a -> Syntax (ParserNetSig f s) (Kleisli Identity) f (Either f (b, f))) ->
  Syntax (ParserNetSig f s) (Kleisli Identity) f (Either f (b, f))
bindN m k = Op (R (R (R (R (R (R (R (R (Bind m k)))))))))

-- ---------------------------------------------------------------------------
-- Copy / discard instances for the base arrow
--
-- Parser plumbing only ever uses 'copy' and 'discard' (to thread the input
-- stream).  'SigMergeZero' is present in the signature because 'AlgNet'
-- includes it, but no plus/zero nodes are ever constructed, so no corresponding
-- instance is required.
-- ---------------------------------------------------------------------------

instance Dg.Copy (Kleisli Identity) a where
  copy = Kleisli $ \a -> Identity (a, a)

instance Dg.Discard (Kleisli Identity) a where
  discard = Kleisli $ \_ -> Identity ()

-- ---------------------------------------------------------------------------
-- Execution algebra
-- ---------------------------------------------------------------------------

-- | Structural nodes map to their counterparts in @Loop Either (Kleisli m)@.
-- The embedding runs the pure @Kleisli Identity@ generators in the target
-- monad; 'SigEitherMap' becomes @Either@ strength, and 'SigEitherCase'
-- becomes a lifted conditional.
instance
  (Monad m, Traced Either (Kleisli m)) =>
  Algebra (SigEitherFunctor f) (Kleisli Identity) (C.Loop Either (Kleisli m))
  where
  type
    Ctx (SigEitherFunctor f) (Kleisli Identity) (C.Loop Either (Kleisli m)) =
      (Monad m, Traced Either (Kleisli m))
  alg _ rec (EitherMap g) = strengthD (rec g)

instance
  (Monad m) =>
  Algebra SigEitherCase (Kleisli Identity) (C.Loop Either (Kleisli m))
  where
  type Ctx SigEitherCase (Kleisli Identity) (C.Loop Either (Kleisli m)) = (Monad m)
  alg _ rec (EitherCase f g) =
    C.Lift $
      Kleisli $
        \case
          Left x -> runKleisli (C.run (rec f)) x
          Right y -> runKleisli (C.run (rec g)) y

instance
  (Monad m, Uncons f s, Traced Either (Kleisli m)) =>
  Algebra (SigBind f s) (Kleisli Identity) (C.Loop Either (Kleisli m))
  where
  type
    Ctx (SigBind f s) (Kleisli Identity) (C.Loop Either (Kleisli m)) =
      (Monad m, Uncons f s, Traced Either (Kleisli m))
  alg _ rec (Bind m k) = bindLoop (rec m) (rec . k)
    where
      bindLoop ::
        forall a b.
        C.Loop Either (Kleisli m) f (Either f (a, f)) ->
        (a -> C.Loop Either (Kleisli m) f (Either f (b, f))) ->
        C.Loop Either (Kleisli m) f (Either f (b, f))
      bindLoop m' k' =
        bindThese m' (\a -> k' a .> C.Lift (Kleisli (pure . eitherToThese)))
          .> C.Lift (Kleisli (pure . theseToEither @f @s @b))
        where
          bindThese ::
            forall a' b'.
            C.Loop Either (Kleisli m) f (Either f (a', f)) ->
            (a' -> C.Loop Either (Kleisli m) f (These b' f)) ->
            C.Loop Either (Kleisli m) f (These b' f)
          bindThese m'' k'' =
            C.Lift $ Kleisli $ \s0 -> do
              runKleisli (C.run m'') s0 >>= \case
                Left s1 -> pure (That s1)
                Right (a, s1) -> runKleisli (C.run (k'' a)) s1

-- ---------------------------------------------------------------------------
-- Pure helper arrows
-- ---------------------------------------------------------------------------

-- | Convert the net result type to the parser result type.
eitherToThese :: Either f (a, f) -> These a f
eitherToThese (Left f) = That f
eitherToThese (Right (a, f)) = These a f

-- | Convert the parser result type back to the net result type.
theseToEither :: forall f s a. (Uncons f s) => These a f -> Either f (a, f)
theseToEither (This a) = Right (a, nil @f @s)
theseToEither (These a f) = Right (a, f)
theseToEither (That f) = Left f

-- | Apply a function to the value part of a success.
applyValue :: (a -> b) -> Either f (a, f) -> Either f (b, f)
applyValue g (Right (a, f)) = Right (g a, f)
applyValue _ (Left f) = Left f

-- | Distribute a product over a coproduct: @(x, Either y z) -> Either (x, y) (x, z)@.
distRight :: (x, Either y z) -> Either (x, y) (x, z)
distRight (x, Left y) = Left (x, y)
distRight (x, Right z) = Right (x, z)

-- | Collapse nested failure: @Either f (Either f x) -> Either f x@.
joinEither :: Either f (Either f x) -> Either f x
joinEither (Left f) = Left f
joinEither (Right (Left f)) = Left f
joinEither (Right (Right x)) = Right x

-- | Leftward associator for pairs.
assocLPair :: (a, (b, c)) -> ((a, b), c)
assocLPair (a, (b, c)) = ((a, b), c)

-- ---------------------------------------------------------------------------
-- Primitive constructors
-- ---------------------------------------------------------------------------

nextN :: forall f s. (Uncons f s) => ParserNet f s s
nextN = ParserNet $ liftK $ \input -> case uncons @f @s input of
  That rest -> Left rest
  This s' -> Right (s', nil @f @s)
  These s' rest -> Right (s', rest)

anyTokenN :: forall f s. (Uncons f s) => ParserNet f s s
anyTokenN = nextN

satisfyN :: forall f s. (Uncons f s) => (s -> Bool) -> ParserNet f s s
satisfyN p = ParserNet $ liftK $ \input -> case uncons @f @s input of
  That rest -> Left rest
  This s' -> if p s' then Right (s', nil @f @s) else Left input
  These s' rest -> if p s' then Right (s', rest) else Left input

-- | Match a specific element.
--
-- >>> runParserNetIdentity (charN 'a' :: ParserNet String Char Char) "abc"
-- These 'a' "bc"
--
-- >>> runParserNetIdentity (charN 'x' :: ParserNet String Char Char) "abc"
-- That "abc"
charN :: forall f s. (Eq s, Uncons f s) => s -> ParserNet f s s
charN c = satisfyN (== c)

-- | Match a sequence of elements.
--
-- >>> runParserNetIdentity (stringN "ab" :: ParserNet String Char String) "abc"
-- These "ab" "c"
--
-- >>> runParserNetIdentity (stringN "ab" :: ParserNet String Char String) "ab"
-- These "ab" ""
stringN :: forall f s. (Eq s, Uncons f s) => [s] -> ParserNet f s [s]
stringN = foldr (\c -> (<*>) ((:) <$> charN c)) (ParserNet $ liftK $ \f -> Right ([], f))

endOfInputN :: forall f s. (Uncons f s) => ParserNet f s ()
endOfInputN = ParserNet $ liftK $ \input -> case uncons @f @s input of
  That _ -> Right ((), input)
  _ -> Left input

takeRestN :: forall f s. (Uncons f s) => ParserNet f s f
takeRestN = ParserNet $ liftK $ \input -> Right (input, nil @f @s)

-- ---------------------------------------------------------------------------
-- Instances
-- ---------------------------------------------------------------------------

instance (Uncons f s) => Functor (ParserNet f s) where
  fmap g (ParserNet p) = ParserNet $ composeN (liftK (applyValue g)) p

instance (Uncons f s) => Applicative (ParserNet f s) where
  pure a = ParserNet $ liftK $ \f -> Right (a, f)

  (<*>) :: forall a b. ParserNet f s (a -> b) -> ParserNet f s a -> ParserNet f s b
  ParserNet pf <*> ParserNet pa =
    ParserNet $
      copyN
        .>> parN pf (liftK id)
        .>> swapN
        .>> liftK distRight
        .>> liftK (first fst)
        .>> mapRightN h
        .>> liftK joinEither
    where
      h =
        liftK assocLPair
          .>> parN (liftK id) pa
          .>> liftK distRight
          .>> liftK (bimap (fst . fst) (\((_, g), (a, f'')) -> (g a, f'')))

instance (Uncons f s) => Monad (ParserNet f s) where
  ParserNet m >>= k = ParserNet $ bindN m (unParserNet . k)

instance (Uncons f s) => Alternative (ParserNet f s) where
  empty = ParserNet $ liftK Left

  -- \| Traced choice over @Either@.
  --
  -- >>> runParserNetIdentity (stringN "ab" <|> stringN "a" :: ParserNet String Char String) "ab"
  -- These "ab" ""
  --
  -- >>> runParserNetIdentity (stringN "ab" <|> stringN "a" :: ParserNet String Char String) "a"
  -- These "a" ""
  --
  -- >>> runParserNetIdentity (stringN "ab" <|> stringN "a" :: ParserNet String Char String) "x"
  -- That "x"
  (<|>) :: forall a. ParserNet f s a -> ParserNet f s a -> ParserNet f s a
  ParserNet p1 <|> ParserNet p2 = ParserNet $ knotN body
    where
      p1' :: Syntax (ParserNetSig f s) (Kleisli Identity) f (Either f (Either f (a, f)))
      p1' = p1 .>> liftK (second Right)

      p2' :: Syntax (ParserNetSig f s) (Kleisli Identity) f (Either f (Either f (a, f)))
      p2' = p2 .>> liftK Right

      body :: Syntax (ParserNetSig f s) (Kleisli Identity) (Either f f) (Either f (Either f (a, f)))
      body = eitherCaseN p2' p1'

instance (Uncons f s) => MonadPlus (ParserNet f s)

-- ---------------------------------------------------------------------------
-- Repetition
-- ---------------------------------------------------------------------------

-- | Zero or more repetitions.  Built with a single knot that carries the
-- accumulated list around the feedback channel.
--
-- >>> runParserNetIdentity (manyN (charN 'a') :: ParserNet String Char String) "aaab"
-- These "aaa" "b"
manyN :: forall f s a. (Uncons f s) => ParserNet f s a -> ParserNet f s [a]
manyN p = ParserNet $ knotN body .>> liftK Right
  where
    runP :: Syntax (ParserNetSig f s) (Kleisli Identity) (f, [a]) (Either (f, [a]) ([a], f))
    runP =
      swapN
        .>> parN (liftK id) (unParserNet p)
        .>> liftK distRight
        .>> liftK
          ( \case
              Left (as, f') -> Right (as, f')
              Right (as, (a, f')) -> Left (f', a : as)
          )

    body :: Syntax (ParserNetSig f s) (Kleisli Identity) (Either (f, [a]) f) (Either (f, [a]) ([a], f))
    body = eitherCaseN runP (liftK (,[]) .>> runP)

-- | One or more repetitions.
--
-- >>> runParserNetIdentity (someN (charN 'a') :: ParserNet String Char String) "aaab"
-- These "aaa" "b"
someN :: (Uncons f s) => ParserNet f s a -> ParserNet f s [a]
someN p = (:) <$> p <*> manyN p

-- ---------------------------------------------------------------------------
-- Execution
-- ---------------------------------------------------------------------------

-- | Execute a 'ParserNet' in an arbitrary base monad.
runParserNet ::
  (Monad m, Uncons f s, Traced Either (Kleisli m)) =>
  ParserNet f s a ->
  Parser m f s a
runParserNet p =
  Parser $
    evalInto
      (\(Kleisli g) -> C.Lift $ Kleisli $ \x -> pure (runIdentity (g x)))
      (unParserNet p)
      .> C.Lift (Kleisli (pure . eitherToThese))

-- | Execute a 'ParserNet' with the identity monad.
runParserNetIdentity ::
  (Uncons f s) =>
  ParserNet f s a ->
  f ->
  These a f
runParserNetIdentity p = PU.runParserIdentity (runParserNet p)
