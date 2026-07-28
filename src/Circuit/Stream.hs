{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

-- | Stream algebra and coalgebra for token streams.
--
-- This module holds the neutral stream interface used by both parsers and
-- agents: 'Uncons' destructs a stream, 'Snoc' constructs one.  It knows nothing
-- about parse results, posts, or agents — only about streams @f@ of tokens
-- @s@.
module Circuit.Stream
  ( -- * Boundary result
    These (..),

    -- * Stream coalgebra
    Uncons (..),

    -- * Stream algebra (dual)
    Snoc (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.These (These (..))
import Data.Word (Word8)

-- | Stream coalgebra with explicit boundary.
--
-- @uncons [x] = This x@ announces the final element at extraction. The
-- 'nil' value is the stream-specific empty used to continue after a 'This'
-- result.
class Uncons f s where
  uncons :: f -> These s f
  nil :: f

-- | Stream algebra: construct a stream by appending one token on the right.
--
-- This is the construction dual of 'Uncons'.  Together they let code move
-- back and forth between tokens and streams using the same coalgebra.
class Snoc f s where
  -- | Append one token to the right of a stream.
  snoc :: f -> s -> f
  -- | The empty stream.
  snocNil :: f

instance Uncons [a] a where
  uncons [] = That []
  uncons [x] = This x
  uncons (x : xs) = These x xs
  nil = []

instance Snoc [a] a where
  snoc xs x = xs ++ [x]
  snocNil = []

instance Uncons ByteString Char where
  uncons bs' = case B.uncons bs' of
    Nothing -> That bs'
    Just (w, rest)
      | B.null rest -> This (w2c w)
      | otherwise -> These (w2c w) rest
    where
      w2c = toEnum . fromIntegral
  nil = B.empty

instance Uncons ByteString Word8 where
  uncons bs' = case B.uncons bs' of
    Nothing -> That bs'
    Just (w, rest)
      | B.null rest -> This w
      | otherwise -> These w rest
  nil = B.empty

instance Uncons Text Char where
  uncons t = case T.uncons t of
    Nothing -> That t
    Just (c, rest)
      | T.null rest -> This c
      | otherwise -> These c rest
  nil = T.empty
