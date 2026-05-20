{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Fast imperative lexers over "ByteString".
--
-- These are complementary to the parser-combinator approach in
-- "Circuit.Parser": they trade compositionality for raw speed
-- (zero-copy slices, "unsafeIndex" loops). The markup lexer can
-- also be viewed as a state machine (see 'oAccumStep' / 'stepMarkupS')
-- which morphs cleanly into a 'Circuit' when composition is needed.
module Circuit.Parser.Lexer
  ( -- * Word lexing
    runWordLexerBS,
    wordFreqBS,

    -- * Markup lexing
    MarkupCtx (..),
    MarkupToken (..),
    runMarkupLexerBS,
    runMarkupStateBS,

    -- * Byte classification
    ByteClass (..),
    classifyByte,

    -- * Accumulator state
    AccState (..),
    accumStep,

    -- * Traced / state-machine interface
    WI (..),
    OAccState (..),
    initOAccState,
    oAccumStep,
    stepMarkupS,
  )
where

import Control.DeepSeq (NFData)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.Word (Word8)
import GHC.Generics (Generic)
import Prelude hiding (id, (.))
import Prelude qualified as P

-- | Strict unboxed pair of byte and index.
data WI = WI {-# UNPACK #-} !Word8 {-# UNPACK #-} !Int

isAlphaW :: Word8 -> Bool
isAlphaW w = (w >= 65 && w <= 90) || (w >= 97 && w <= 122)

toLowerW :: Word8 -> Word8
toLowerW w
  | w >= 65 && w <= 90 = w + 32
  | otherwise = w

-- | Run word lexer directly over a ByteString.
runWordLexerBS :: ByteString -> [ByteString]
runWordLexerBS bs = go 0 False 0
  where
    !len = BS.length bs
    go !i !inWord !start
      | i >= len = if inWord then [lowerSlice start (i - start)] else []
      | otherwise =
          let !w = BSU.unsafeIndex bs i
           in if isAlphaW w
                then go (i + 1) True (if inWord then start else i)
                else
                  if inWord
                    then lowerSlice start (i - start) : go (i + 1) False 0
                    else go (i + 1) False 0
    lowerSlice s l = BS.map toLowerW (BSU.unsafeTake l (BSU.unsafeDrop s bs))

-- | Word frequency list — single pass, accumulate (word, count) pairs.
wordFreqBS :: ByteString -> [(ByteString, Int)]
wordFreqBS bs = go 0 False 0 []
  where
    !len = BS.length bs
    go !i !inWord !start !acc
      | i >= len =
          if inWord
            then insertFreq (lowerSlice start (i - start)) acc
            else acc
      | otherwise =
          let !w = BSU.unsafeIndex bs i
           in if isAlphaW w
                then go (i + 1) True (if inWord then start else i) acc
                else
                  if inWord
                    then go (i + 1) False 0 (insertFreq (lowerSlice start (i - start)) acc)
                    else go (i + 1) False 0 acc
    lowerSlice s l = BS.map toLowerW (BSU.unsafeTake l (BSU.unsafeDrop s bs))
    insertFreq word [] = [(word, 1)]
    insertFreq word ((w, c) : rest)
      | word == w = (w, c + 1) : rest
      | otherwise = (w, c) : insertFreq word rest

data MarkupCtx
  = InContent
  | InTagName
  | InAttr
  | InAttrVal Char
  | InClose
  | InComment
  deriving (Eq, Show, Generic)

instance NFData MarkupCtx

-- | Markup token. ByteString fields are zero-copy slices of the input.
data MarkupToken
  = TOpenTag ByteString
  | TCloseTag ByteString
  | TContent ByteString
  | TSelfClose
  | TTagEnd
  | TAttrName ByteString
  | TAttrVal ByteString
  | TComment ByteString
  deriving (Eq, Show, Generic)

instance NFData MarkupToken

data ByteClass
  = BLt
  | BGt
  | BSlash
  | BEquals
  | BQuote Char
  | BSpace
  | BAlpha Word8
  | BDash
  | BBang
  | BQuestion
  deriving (Eq, Show)

classifyByte :: MarkupCtx -> Word8 -> ByteClass
classifyByte _ w = case w of
  60 -> BLt
  62 -> BGt
  47 -> BSlash
  61 -> BEquals
  39 -> BQuote '\''
  34 -> BQuote '"'
  32 -> BSpace
  9 -> BSpace
  10 -> BSpace
  13 -> BSpace
  45 -> BDash
  33 -> BBang
  63 -> BQuestion
  _ -> BAlpha w

-- | Accumulator: track start offset and length of current token being built.
data AccState = AccState
  { accStart :: {-# UNPACK #-} !Int,
    accLen :: {-# UNPACK #-} !Int,
    accCtx :: !MarkupCtx
  }

-- | Step the accumulator given a byte class, current context, and byte index.
accumStep ::
  AccState ->
  ByteClass ->
  Int ->
  (Maybe (ByteString -> MarkupToken, Int, Int), AccState)
accumStep (AccState !s !l !ctx) bc !i = case (ctx, bc) of
  (InContent, BLt) ->
    ( if l == 0 then Nothing else Just (TContent, s, l),
      AccState i 0 InTagName
    )
  (InContent, _) ->
    (Nothing, AccState (if l == 0 then i else s) (l + 1) InContent)
  (InTagName, BSpace) ->
    ( if l == 0 then Nothing else Just (TOpenTag, s, l),
      AccState i 0 InAttr
    )
  (InTagName, BGt) ->
    ( if l == 0 then Nothing else Just (TOpenTag, s, l),
      AccState i 0 InContent
    )
  (InTagName, BSlash) ->
    if l == 0
      then (Nothing, AccState i 0 InClose)
      else (Nothing, AccState s (l + 1) InTagName)
  (InTagName, _) ->
    (Nothing, AccState (if l == 0 then i else s) (l + 1) InTagName)
  (InAttr, BGt) ->
    (Just (P.const TTagEnd, 0, 0), AccState i 0 InContent)
  (InAttr, BSlash) ->
    (Just (P.const TSelfClose, 0, 0), AccState i 0 InContent)
  (InAttr, BSpace) ->
    (Nothing, AccState i 0 InAttr)
  (InAttr, _) ->
    (Nothing, AccState (if l == 0 then i else s) (l + 1) InAttr)
  (InClose, BGt) ->
    ( if l == 0 then Nothing else Just (TCloseTag, s, l),
      AccState i 0 InContent
    )
  (InClose, BSpace) ->
    (Nothing, AccState s l InClose)
  (InClose, _) ->
    (Nothing, AccState (if l == 0 then i else s) (l + 1) InClose)
  _ ->
    (Nothing, AccState (if l == 0 then i else s) (l + 1) ctx)

-- | Run markup lexer directly over a ByteString.
runMarkupLexerBS :: ByteString -> [MarkupToken]
runMarkupLexerBS bs = go 0 (AccState 0 0 InContent)
  where
    !len = BS.length bs
    go !i !acc
      | i >= len = []
      | otherwise =
          let !w = BSU.unsafeIndex bs i
              !bc = classifyByte (accCtx acc) w
              (emit, !acc') = accumStep acc bc i
           in case emit of
                Nothing -> go (i + 1) acc'
                Just (con, s, l) ->
                  let !tok =
                        if l == 0
                          then con BS.empty
                          else con (BSU.unsafeTake l (BSU.unsafeDrop s bs))
                   in tok : go (i + 1) acc'

-- | Offset-tracking accumulator state for the Traced pipeline.
data OAccState = OAccState
  { _oStart :: {-# UNPACK #-} !Int,
    _oLen :: {-# UNPACK #-} !Int,
    oCtx :: !MarkupCtx
  }

-- | Step the offset accumulator.
oAccumStep ::
  OAccState ->
  (ByteClass, Int, MarkupCtx) ->
  (Maybe (ByteString -> MarkupToken, Int, Int), OAccState)
oAccumStep (OAccState !s !l !ctx) (bc, !i, _) = case (ctx, bc) of
  (InContent, BLt) ->
    ( if l == 0 then Nothing else Just (TContent, s, l),
      OAccState i 0 InTagName
    )
  (InContent, _) ->
    (Nothing, OAccState (if l == 0 then i else s) (l + 1) InContent)
  (InTagName, BSpace) ->
    ( if l == 0 then Nothing else Just (TOpenTag, s, l),
      OAccState i 0 InAttr
    )
  (InTagName, BGt) ->
    ( if l == 0 then Nothing else Just (TOpenTag, s, l),
      OAccState i 0 InContent
    )
  (InTagName, BSlash) ->
    if l == 0
      then (Nothing, OAccState i 0 InClose)
      else (Nothing, OAccState s (l + 1) InTagName)
  (InTagName, _) ->
    (Nothing, OAccState (if l == 0 then i else s) (l + 1) InTagName)
  (InAttr, BGt) ->
    (Just (P.const TTagEnd, 0, 0), OAccState i 0 InContent)
  (InAttr, BSlash) ->
    (Just (P.const TSelfClose, 0, 0), OAccState i 0 InContent)
  (InAttr, BSpace) ->
    (Nothing, OAccState i 0 InAttr)
  (InAttr, _) ->
    (Nothing, OAccState (if l == 0 then i else s) (l + 1) InAttr)
  (InClose, BGt) ->
    ( if l == 0 then Nothing else Just (TCloseTag, s, l),
      OAccState i 0 InContent
    )
  (InClose, BSpace) -> (Nothing, OAccState s l InClose)
  (InClose, _) ->
    (Nothing, OAccState (if l == 0 then i else s) (l + 1) InClose)
  _ ->
    (Nothing, OAccState (if l == 0 then i else s) (l + 1) ctx)

stage1S :: (WI, OAccState) -> ((ByteClass, Int), OAccState)
stage1S (WI w i, acc) = ((classifyByte (oCtx acc) w, i), acc)

stage2S :: ((ByteClass, Int), OAccState) -> (Maybe (ByteString -> MarkupToken, Int, Int), OAccState)
stage2S ((bc, i), acc) = oAccumStep acc (bc, i, oCtx acc)

-- | Compiled step function.
stepMarkupS :: WI -> OAccState -> (Maybe (ByteString -> MarkupToken, Int, Int), OAccState)
stepMarkupS wi acc = stage2S (stage1S (wi, acc))

-- | Initial accumulator state.
initOAccState :: OAccState
initOAccState = OAccState 0 0 InContent

-- | Run via Traced (->) State pipeline.
runMarkupStateBS :: ByteString -> [MarkupToken]
runMarkupStateBS bs
  | BS.null bs = []
  | otherwise =
      let !w0 = BSU.unsafeHead bs
          (mEmit, !s0) = stepMarkupS (WI w0 0) initOAccState
          toks0 = case mEmit of
            Nothing -> []
            Just (con, start, len) -> [mkTok con start len]
       in toks0 ++ go s0 1 (BSU.unsafeTail bs)
  where
    go !acc !i bs'
      | BS.null bs' = []
      | otherwise =
          let !w = BSU.unsafeHead bs'
              (mEmit, !acc') = stepMarkupS (WI w i) acc
           in case mEmit of
                Nothing ->
                  go acc' (i + 1) (BSU.unsafeTail bs')
                Just (con, start, len) ->
                  mkTok con start len : go acc' (i + 1) (BSU.unsafeTail bs')
    mkTok con start len
      | len == 0 = con BS.empty
      | otherwise = con (BSU.unsafeTake len (BSU.unsafeDrop start bs))
