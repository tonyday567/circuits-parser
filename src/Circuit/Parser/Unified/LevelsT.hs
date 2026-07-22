{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Fair breadth-first parser family via the @LevelsT@ transformer.
--
-- This is the Kidney & Wu "Algebras for Weighted Search" BFS layer applied to
-- the unified parser.  @LevelsT m a@ is a list of bags of @a@, ordered by the
-- number of steps needed to reach each outcome.  The 'Alternative' '<|>'
-- interleaves outcomes at the same level rather than left-biasing, and the
-- 'Traced' instance for @Kleisli (LevelsT m)@ enumerates /all/ reachable
-- 'Right' exits of an @Either@ loop instead of committing to the first one.
--
-- The standard 'Circuit.Parser.Unified.<|>' still commits on the first success,
-- because it does not emit a failure-feedback edge after a success.  Use
-- 'altLevels' for a choice operator that runs both branches and interleaves
-- their results fairly.
--
-- === doctests
--
-- >>> import Circuit.Parser.Unified
-- >>> import Circuit.Parser.Unified.LevelsT
-- >>> import Data.Functor.Identity (Identity, runIdentity)
-- >>> import Data.These (These (..))
--
-- >>> let p = string "ab" `altLevels` string "a" :: Parser (LevelsT Identity) String Char String
-- >>> runIdentity $ runParserLevelsT p "ab"
-- [These "ab" "",These "a" "b"]
module Circuit.Parser.Unified.LevelsT
  ( -- * Levels transformer
    LevelsT (..),
    Bag,
    observeLevelsT,

    -- * Fair parser choice and runner
    altLevels,
    runParserLevelsT,
  )
where

import Circuit qualified as C
import Circuit.Channel (Traced (..))
import Circuit.Parser.Unified
  ( Parser (..),
    Uncons,
    runParser,
  )
import Control.Applicative (Alternative (..))
import Control.Arrow (Kleisli (..))
import Control.Category ((.))
import Control.Monad (MonadPlus, ap)
import Control.Monad.Trans (MonadTrans (..))
import Data.Bifunctor (bimap)
import Data.These (These (..))
import Prelude hiding (id, (.))

-- | A bag is an unordered multiset.  We represent it as a list; ordering
-- within a bag is not semantically significant.
type Bag a = [a]

-- | The levels transformer: a breadth-first list of bags, each bag holding
-- the outcomes reachable in the same number of steps.
--
-- This is the direct (non-Cayley) representation from Kidney & Wu.  The
-- efficient zip-like '<|>' can be implemented with hyperfunctions; here we
-- keep the structure explicit so the semantics are easy to read.
newtype LevelsT m a = LevelsT
  { runLevelsT :: m (Maybe (Bag a, LevelsT m a))
  }

-- | Extract the outcomes level by level, flattening each bag in order.
observeLevelsT :: (Monad m) => LevelsT m a -> m [a]
observeLevelsT (LevelsT xs) = xs >>= go
  where
    go Nothing = pure []
    go (Just (bag, rest)) = (bag ++) <$> observeLevelsT rest

-- ---------------------------------------------------------------------------
-- LevelsT instances
-- ---------------------------------------------------------------------------

instance (Functor m) => Functor (LevelsT m) where
  fmap f (LevelsT xs) =
    LevelsT $ fmap (fmap (bimap (map f) (fmap f))) xs

instance (Monad m) => Applicative (LevelsT m) where
  pure a = LevelsT (pure (Just ([a], emptyLevelsT)))
  (<*>) = ap

instance (Monad m) => Monad (LevelsT m) where
  LevelsT xs >>= k = LevelsT (xs >>= go)
    where
      go Nothing = pure Nothing
      go (Just (bag, rest)) =
        runLevelsT (choices k bag `altLevelsT` wrapLevelsT (rest >>= k))

choices :: (Monad m) => (a -> LevelsT m b) -> Bag a -> LevelsT m b
choices k = foldr (\a acc -> k a `altLevelsT` acc) emptyLevelsT

instance (Monad m) => Alternative (LevelsT m) where
  empty = emptyLevelsT
  (<|>) = altLevelsT

instance (Monad m) => MonadPlus (LevelsT m)

instance MonadTrans LevelsT where
  lift m = LevelsT (fmap (\a -> Just ([a], emptyLevelsT)) m)

emptyLevelsT :: (Monad m) => LevelsT m a
emptyLevelsT = LevelsT (pure Nothing)

wrapLevelsT :: (Monad m) => LevelsT m a -> LevelsT m a
wrapLevelsT xs = LevelsT (pure (Just ([], xs)))

altLevelsT :: (Monad m) => LevelsT m a -> LevelsT m a -> LevelsT m a
altLevelsT (LevelsT xs) (LevelsT ys) = LevelsT (liftA2 go xs ys)
  where
    go Nothing y = y
    go x Nothing = x
    go (Just (bagX, restX)) (Just (bagY, restY)) =
      Just (bagX ++ bagY, altLevelsT restX restY)

-- | Split a 'LevelsT' into its first outcome and the remainder.
msplitLevelsT :: (Monad m) => LevelsT m a -> m (Maybe (a, LevelsT m a))
msplitLevelsT (LevelsT xs) = xs >>= go
  where
    go Nothing = pure Nothing
    go (Just ([], rest)) = msplitLevelsT rest
    go (Just (a : as, rest)) =
      pure (Just (a, LevelsT (pure (Just (as, rest)))))

-- ---------------------------------------------------------------------------
-- Branching Either trace for LevelsT
-- ---------------------------------------------------------------------------

-- | Enumerate every 'Right' result reachable by following 'Left' feedback
-- edges, interleaving the search breadth-first via 'LevelsT'.
instance (Monad m) => Traced Either (Kleisli (LevelsT m)) where
  trace (Kleisli f) = Kleisli (go . Right)
    where
      go x = do
        mb <- lift (msplitLevelsT (f x))
        case mb of
          Nothing -> empty
          Just (r, rest) -> handle r `altLevelsT` (rest >>= handle)

      handle (Left a) = go (Left a)
      handle (Right c) = pure c

-- ---------------------------------------------------------------------------
-- Fair parser choice and runner
-- ---------------------------------------------------------------------------

-- | Fair choice: emit every success from the left parser, and also fall
-- through to the right parser.  The two streams are interleaved level by
-- level.
altLevels ::
  (Monad m, Uncons f s) =>
  Parser (LevelsT m) f s a ->
  Parser (LevelsT m) f s a ->
  Parser (LevelsT m) f s a
altLevels p1 p2 = Parser $ C.trace $ C.Lift $ Kleisli $ \case
  Right s -> leftSuccesses s `altLevelsT` pure (Left s)
  Left s -> Right <$> runParser p2 s
  where
    leftSuccesses s = do
      res <- runParser p1 s
      case res of
        That _ -> empty
        _ -> pure (Right res)

-- | Run a parser with fair breadth-first enumeration and collect every
-- result.
runParserLevelsT ::
  (Monad m, Uncons f s) =>
  Parser (LevelsT m) f s a ->
  f ->
  m [These a f]
runParserLevelsT p f = observeLevelsT (runParser p f)
