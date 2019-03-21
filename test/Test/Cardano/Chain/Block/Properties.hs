{-# LANGUAGE TemplateHaskell #-}

module Test.Cardano.Chain.Block.Properties
  ( tests
  )
where

import Cardano.Prelude
import Test.Cardano.Prelude

import Hedgehog (Property, withTests, property, forAll, checkSequential, discover)

import Cardano.Chain.Block (headerPrevHash, headerSlot)
import Cardano.Chain.Slotting (EpochSlots (..), isZerothSlotId)

import Test.Cardano.Chain.Block.Gen (genHeader)
import Test.Cardano.Crypto.Gen (genProtocolMagicId)

-- Assert that 'headerPrevHash' returns 'Nothing' when the current slot is
-- the zeroth SlotId.
prop_headerPrevHash_genesis_Nothing :: Property
prop_headerPrevHash_genesis_Nothing =
  withTests 100 . property $ do
    pm <- forAll genProtocolMagicId
    hdr <- forAll $ genHeader pm (EpochSlots 20000)
    if isZerothSlotId (headerSlot hdr)
      then assertIsNothing $ headerPrevHash hdr
      else assertIsJust $ headerPrevHash hdr


tests :: IO Bool
tests = checkSequential $$(discover)
