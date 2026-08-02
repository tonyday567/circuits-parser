{-# LANGUAGE BangPatterns #-}

-- | Unescaping of JSON string literals.
--
-- The input is the raw slice between the quotes, escapes intact. Output is
-- decoded 'Text'. The pure-Haskell equivalent of aeson's @unescapeText@,
-- minus the C fast path: no escapes means one 'decodeUtf8'' and done.
module Circuit.Parser.Json.Unescape
  ( unescape,
  )
where

import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Char (chr, isDigit, isHexDigit, ord)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8')
import Data.Word (Word8)

-- $setup
-- >>> import Data.ByteString.Char8 qualified as C

-- | Unescape a raw JSON string slice (the bytes between the quotes) to
-- 'Text'.
--
-- >>> unescape (C.pack "hello")
-- Right "hello"
--
-- >>> unescape (C.pack "a\\nb")
-- Right "a\nb"
--
-- >>> unescape (C.pack "\\u0041\\u00e9")
-- Right "A\233"
--
-- >>> unescape (C.pack "\\uD834\\uDD1E")
-- Right "\119070"
--
-- >>> unescape (C.pack "bad\\x")
-- Left "invalid escape: \\x"
unescape :: ByteString -> Either String Text
unescape bs = case B.elemIndex backslash bs of
  Nothing
    | B.any (< 0x20) bs -> Left "unescaped control character"
    | otherwise -> either (Left . show) Right (decodeUtf8' bs)
  Just _ -> go 0 []
  where
    !len = B.length bs
    go !i acc
      | i >= len =
          Right (T.concat (reverse acc))
      | otherwise =
          case elemIndexFrom backslash i bs of
            Nothing ->
              -- tail chunk, no more escapes
              let chunk = B.drop i bs
               in if B.any (< 0x20) chunk
                    then Left "unescaped control character"
                    else case decodeUtf8' chunk of
                      Left e -> Left (show e)
                      Right t -> Right (T.concat (reverse (t : acc)))
            Just j ->
              -- chunk [i, j) is escape-free raw UTF-8
              let chunk = B.take (j - i) (B.drop i bs)
               in if B.any (< 0x20) chunk
                    then Left "unescaped control character"
                    else case decodeUtf8' chunk of
                      Left e -> Left (show e)
                      Right t -> escapeAt (j + 1) (t : acc)
    -- i points just past the backslash
    escapeAt !i acc
      | i >= len = Left "trailing backslash"
      | otherwise = case B.index bs i of
          0x22 -> go (i + 1) (T.singleton '"' : acc)
          0x5C -> go (i + 1) (T.singleton '\\' : acc)
          0x2F -> go (i + 1) (T.singleton '/' : acc)
          0x62 -> go (i + 1) (T.singleton '\b' : acc)
          0x66 -> go (i + 1) (T.singleton '\f' : acc)
          0x6E -> go (i + 1) (T.singleton '\n' : acc)
          0x72 -> go (i + 1) (T.singleton '\r' : acc)
          0x74 -> go (i + 1) (T.singleton '\t' : acc)
          0x75 -> hexAt (i + 1) acc
          w -> Left ("invalid escape: \\" ++ [w2c w])
    -- i points at the first of 4 hex digits
    hexAt !i acc
      | i + 4 > len = Left "truncated \\u escape"
      | otherwise =
          let h = B.take 4 (B.drop i bs)
           in if B.all (isHexDigit . w2c) h
                then
                  let !n = hexVal h
                   in if n >= 0xD800 && n <= 0xDBFF
                        then lowSurrogate (i + 4) n acc
                        else
                          if n >= 0xDC00 && n <= 0xDFFF
                            then Left "lone low surrogate"
                            else go (i + 4) (T.singleton (chr n) : acc)
                else Left "invalid hex in \\u escape"
    -- i points just past a high surrogate; expect \uDC00-\uDFFF
    lowSurrogate !i !hi acc
      | i + 6 > len = Left "truncated surrogate pair"
      | B.index bs i /= backslash || B.index bs (i + 1) /= 0x75 =
          Left "lone high surrogate"
      | otherwise =
          let h = B.take 4 (B.drop (i + 2) bs)
           in if B.all (isHexDigit . w2c) h
                then
                  let !lo = hexVal h
                   in if lo >= 0xDC00 && lo <= 0xDFFF
                        then
                          let !n = 0x10000 + ((hi - 0xD800) `shiftL` 10) .|. (lo - 0xDC00)
                           in go (i + 6) (T.singleton (chr n) : acc)
                        else Left "lone high surrogate"
                else Left "invalid hex in \\u escape"

backslash :: Word8
backslash = 0x5C

w2c :: Word8 -> Char
w2c = chr . fromIntegral

-- | elemIndexFrom is not exported by bytestring; search from an offset.
elemIndexFrom :: Word8 -> Int -> ByteString -> Maybe Int
elemIndexFrom w i bs = (i +) <$> B.elemIndex w (B.drop i bs)

hexVal :: ByteString -> Int
hexVal = B.foldl' step 0
  where
    step !acc w = acc * 16 + dig (w2c w)
    dig c
      | isDigit c = ord c - ord '0'
      | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
      | otherwise = ord c - ord 'A' + 10
