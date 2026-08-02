{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Fast imperative JSON tokenizer over strict 'ByteString'.
--
-- Complementary to the combinator path in "Circuit.Parser.Json": it trades
-- compositionality for raw speed — a single 'unsafeIndex' pass emitting
-- zero-copy slices. Structural tokens only; validation (number grammar,
-- escape legality, nesting) belongs to the layer above.
module Circuit.Parser.Json.Lexer
  ( -- * Tokens
    JsonToken (..),

    -- * Running
    runJsonLexerBS,
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

-- | A flat JSON structural token. 'TString' and 'TNumber' payloads are
-- zero-copy slices of the input; a 'TString' keeps its escapes intact.
data JsonToken
  = TBraceOpen
  | TBraceClose
  | TBrackOpen
  | TBrackClose
  | TComma
  | TColon
  | TString ByteString
  | TNumber ByteString
  | TTrue
  | TFalse
  | TNull
  deriving (Eq, Show, Generic)

instance NFData JsonToken

-- | Tokenize a strict 'ByteString'.
--
-- On failure, returns the byte offset and a message.
--
-- >>> runJsonLexerBS (C.pack "{\"a\": [1, true]}")
-- Right [TBraceOpen,TString "a",TColon,TBrackOpen,TNumber "1",TComma,TTrue,TBrackClose,TBraceClose]
--
-- >>> runJsonLexerBS (C.pack "[1")
-- Right [TBrackOpen,TNumber "1"]
--
-- >>> runJsonLexerBS (C.pack "\"ab")
-- Left (3,"unterminated string")
--
-- >>> runJsonLexerBS (C.pack "nul")
-- Left (0,"bad literal")
--
-- >>> runJsonLexerBS (C.pack "@")
-- Left (0,"unexpected byte 64")
runJsonLexerBS :: ByteString -> Either (Int, String) [JsonToken]
runJsonLexerBS bs = go 0
  where
    !len = BS.length bs
    go !i
      | i >= len = Right []
      | otherwise = case BSU.unsafeIndex bs i of
          w
            | isSpaceW w -> go (i + 1)
          0x7B -> emit TBraceOpen i -- {
          0x7D -> emit TBraceClose i -- }
          0x5B -> emit TBrackOpen i -- [
          0x5D -> emit TBrackClose i -- ]
          0x2C -> emit TComma i -- ,
          0x3A -> emit TColon i -- :
          0x22 -> stringAt (i + 1) -- "
          0x74 -> literalAt i "rue" TTrue -- t
          0x66 -> literalAt i "alse" TFalse -- f
          0x6E -> literalAt i "ull" TNull -- n
          w
            | w == 0x2D || isDigitW w -> numberAt i
            | otherwise -> Left (i, "unexpected byte " ++ show w)
    emit t i = (t :) <$> go (i + 1)
    -- i points just past the opening quote
    stringAt !i = case stringEnd i of
      Nothing -> Left (len, "unterminated string")
      Just j -> (TString (slice i j) :) <$> go (j + 1)
    -- index of the closing quote (backslash skips the next byte), or end
    stringEnd !i
      | i >= len = Nothing
      | otherwise = case BSU.unsafeIndex bs i of
          0x5C -> if i + 1 >= len then Nothing else stringEnd (i + 2)
          0x22 -> Just i
          _ -> stringEnd (i + 1)
    numberAt !i =
      let !j = numberEnd i
       in (TNumber (slice i j) :) <$> go j
    numberEnd !i
      | i >= len = i
      | otherwise =
          let w = BSU.unsafeIndex bs i
           in if isDigitW w || w == 0x2D || w == 0x2B || w == 0x2E || w == 0x65 || w == 0x45
                then numberEnd (i + 1)
                else i
    -- i points at the literal's first byte; rest is the remaining bytes
    literalAt !i rest t
      | i + 1 + BS.length rest <= len && BSU.unsafeTake (BS.length rest) (BSU.unsafeDrop (i + 1) bs) == rest =
          (t :) <$> go (i + 1 + BS.length rest)
      | otherwise = Left (i, "bad literal")
    slice s e = BSU.unsafeTake (e - s) (BSU.unsafeDrop s bs)

isSpaceW :: Word8 -> Bool
isSpaceW w = w == 0x20 || w == 0x0A || w == 0x0D || w == 0x09

isDigitW :: Word8 -> Bool
isDigitW w = w >= 0x30 && w <= 0x39
