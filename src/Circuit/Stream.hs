{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

-- | Stream algebra and coalgebra for token streams.
--
-- This module re-exports the neutral stream interface from @circuits@ and
-- supplies the concrete instances for lists, 'ByteString', and 'Text'.
module Circuit.Stream
  ( -- * Boundary result
    These (..),

    -- * Stream coalgebra
    Uncons (..),

    -- * Stream algebra (left construction dual)
    Cons (..),

    -- * Stream algebra (right construction dual)
    Snoc (..),
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import "circuits" Circuit.Stream (Cons (..), Snoc (..), These (..), Uncons (..))

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
