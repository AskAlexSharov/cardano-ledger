{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Serialization primitives built on top of the @ToCBOR@ typeclass

module Cardano.Binary.Serialize
  ( serialize
  , serializeWith
  , serialize'
  , serializeBuilder
  , serializeEncoding

  -- * Temporary functions
  , biSize

  -- * CBOR in CBOR
  , encodeKnownCborDataItem
  , encodeUnknownCborDataItem
  , knownCborDataItemSizeExpr
  , unknownCborDataItemSizeExpr

  -- * Cyclic redundancy check
  , encodeCrcProtected
  , encodedCrcProtectedSizeExpr
  )
where

import Cardano.Prelude

import qualified Codec.CBOR.Encoding as E
import qualified Codec.CBOR.Write as CBOR.Write
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder.Extra as Builder
import qualified Data.ByteString.Lazy as BSL
import Data.Digest.CRC32 (CRC32(..))

import Cardano.Binary.ToCBOR (Size, ToCBOR(..), apMono, withWordSize)


-- | Serialize a Haskell value to an external binary representation.
--
--   The output is represented as a lazy 'LByteString' and is constructed
--   incrementally.
serialize :: ToCBOR a => a -> LByteString
-- 1024 is the size of the first buffer, 4096 is the size of subsequent
-- buffers. Chosen because they seem to give good performance. They are not
-- sacred.
serialize = serializeWith 1024 4096

-- | Serialize a Haskell value to an external binary representation.
--
--   The output is represented as a strict 'ByteString'.
serialize' :: ToCBOR a => a -> ByteString
serialize' = BSL.toStrict . serialize

-- | Serialize into a Builder. Useful if you want to throw other ByteStrings
--   around it.
serializeBuilder :: ToCBOR a => a -> Builder
serializeBuilder = CBOR.Write.toBuilder . toCBOR

-- | Serialize using the safe allocation strategy with a given first and
--   subsequent chunk size.
serializeWith :: ToCBOR a => Int -> Int -> a -> LByteString
serializeWith firstChunk nextChunk =
  Builder.toLazyByteStringWith strategy mempty . serializeBuilder
  where strategy = Builder.safeStrategy firstChunk nextChunk

serializeEncoding :: E.Encoding -> LByteString
serializeEncoding =
  Builder.toLazyByteStringWith strategy mempty . CBOR.Write.toBuilder
  where strategy = Builder.safeStrategy 1024 4096


--------------------------------------------------------------------------------
-- Helper functions
--------------------------------------------------------------------------------

-- | Compute size of something serializable in bytes.
biSize :: ToCBOR a => a -> Natural
biSize = fromIntegral . BSL.length . serialize
{-# INLINE biSize #-}


--------------------------------------------------------------------------------
-- CBORDataItem
-- https://tools.ietf.org/html/rfc7049#section-2.4.4.1
--------------------------------------------------------------------------------

-- | Encode and serialise the given `a` and sorround it with the semantic tag 24
--   In CBOR diagnostic notation:
--   >>> 24(h'DEADBEEF')
encodeKnownCborDataItem :: ToCBOR a => a -> E.Encoding
encodeKnownCborDataItem = encodeUnknownCborDataItem . serialize

-- | Like `encodeKnownCborDataItem`, but assumes nothing about the shape of
--   input object, so that it must be passed as a binary `ByteString` blob. It's
--   the caller responsibility to ensure the input `ByteString` correspond
--   indeed to valid, previously-serialised CBOR data.
encodeUnknownCborDataItem :: LByteString -> E.Encoding
encodeUnknownCborDataItem x = E.encodeTag 24 <> toCBOR x

knownCborDataItemSizeExpr :: Size -> Size
knownCborDataItemSizeExpr x = 2 + apMono "withWordSize" withWordSize x + x

unknownCborDataItemSizeExpr :: Size -> Size
unknownCborDataItemSizeExpr x = 2 + apMono "withWordSize" withWordSize x + x

-- | Encodes a type `a` , protecting it from
--   tampering/network-transport-alteration by protecting it with a CRC.
encodeCrcProtected :: ToCBOR a => a -> E.Encoding
encodeCrcProtected x =
  E.encodeListLen 2 <> encodeUnknownCborDataItem body <> toCBOR (crc32 body)
  where body = serialize x

encodedCrcProtectedSizeExpr
  :: forall a
   . ToCBOR a
  => (forall t . ToCBOR t => Proxy t -> Size)
  -> Proxy a
  -> Size
encodedCrcProtectedSizeExpr size pxy =
  2 + unknownCborDataItemSizeExpr (size pxy) + size
    (pure $ crc32 (serialize (panic "unused" :: a)))
