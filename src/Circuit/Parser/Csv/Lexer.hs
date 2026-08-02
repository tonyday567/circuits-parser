{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Fast imperative CSV tokenizer over strict 'ByteString'.
--
-- Complementary to the combinator path in "Circuit.Parser.Csv": a single
-- 'unsafeIndex' pass emitting zero-copy slices. Commas are implied between
-- adjacent field tokens; 'CRowEnd' marks record boundaries. 'CQuoted'
-- payloads keep their doubled-quote escapes intact.
module Circuit.Parser.Csv.Lexer
  ( -- * Tokens
    CsvToken (..),

    -- * Running
    runCsvLexerBS,
  )
where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Word (Word8)
import GHC.Generics (Generic)

-- $setup
-- >>> import Data.ByteString.Char8 qualified as C

-- | A flat CSV token. Payloads are zero-copy slices of the input.
data CsvToken
  = CField ByteString
  | CQuoted ByteString
  | CRowEnd
  deriving (Eq, Show, Generic)

instance NFData CsvToken

-- | Tokenize a strict 'ByteString' as CSV.
--
-- On failure, returns the byte offset and a message.
--
-- >>> runCsvLexerBS (C.pack "a,b\r\n\"x\"\"y\",z")
-- Right [CField "a",CField "b",CRowEnd,CQuoted "x\"\"y",CField "z"]
--
-- >>> runCsvLexerBS (C.pack "a,\"b")
-- Left (4,"unterminated quoted field")
--
-- >>> runCsvLexerBS (C.pack "ab\"cd")
-- Left (2,"unexpected quote in plain field")
runCsvLexerBS :: ByteString -> Either (Int, String) [CsvToken]
runCsvLexerBS bs = fieldAt 0
  where
    !len = BS.length bs
    fieldAt !i
      | i >= len = Right []
      | BSU.unsafeIndex bs i == quote = quotedAt i
      | otherwise = plainAt i
    plainAt !i =
      let !j = plainEnd i
       in if j < len && BSU.unsafeIndex bs j == quote
            then Left (j, "unexpected quote in plain field")
            else (CField (slice i j) :) <$> sep j
    plainEnd !i
      | i >= len = i
      | otherwise = case BSU.unsafeIndex bs i of
          0x2C -> i -- ,
          0x0D -> i -- CR
          0x0A -> i -- LF
          0x22 -> i -- "
          _ -> plainEnd (i + 1)
    quotedAt !i = case quotedEnd (i + 1) of
      Nothing -> Left (len, "unterminated quoted field")
      Just j -> (CQuoted (slice (i + 1) j) :) <$> sep (j + 1)
    -- index of the closing quote (doubled "" is an escape), or end
    quotedEnd !i
      | i >= len = Nothing
      | otherwise = case BSU.unsafeIndex bs i of
          0x22 ->
            if i + 1 < len && BSU.unsafeIndex bs (i + 1) == quote
              then quotedEnd (i + 2)
              else Just i
          _ -> quotedEnd (i + 1)
    -- after a field: comma, line ending, or end of input
    sep !i
      | i >= len = Right []
      | otherwise = case BSU.unsafeIndex bs i of
          0x2C -> fieldAt (i + 1)
          0x0D -> (CRowEnd :) <$> fieldAt (if i + 1 < len && BSU.unsafeIndex bs (i + 1) == 0x0A then i + 2 else i + 1)
          0x0A -> (CRowEnd :) <$> fieldAt (i + 1)
          w -> Left (i, "unexpected byte " ++ show w)
    slice s e = BSU.unsafeTake (e - s) (BSU.unsafeDrop s bs)

quote :: Word8
quote = 0x22
