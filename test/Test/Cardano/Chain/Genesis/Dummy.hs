{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Test.Cardano.Chain.Genesis.Dummy
  ( dummyConfig
  , dummyConfigStartTime
  , dummyK
  , dummyEpochSlots
  , dummySlotSecurityParam
  , dummyGenesisInitializer
  , dummyGenesisAvvmBalances
  , dummyGeneratedSecrets
  , dummyGenesisSecretKeys
  , dummyGenesisSecretKeysRich
  , dummyGenesisSecretKeysPoor
  , dummyGenesisSecretsRich
  , dummyGenesisSecretsPoor
  , dummyGenesisSpec
  , dummyProtocolParameters
  , dummyGenesisData
  , dummyGenesisDataStartTime
  , dummyGenesisHash
  )
where

import Cardano.Prelude

import Data.Time (Day(..), UTCTime(..))

import Cardano.Chain.Common
  ( BlockCount
  , TxFeePolicy(..)
  , TxSizeLinear(..)
  , mkKnownLovelace
  , mkKnownLovelacePortion
  )
import Cardano.Chain.Genesis
  ( Config(..)
  , FakeAvvmOptions(..)
  , GeneratedSecrets(..)
  , GenesisAvvmBalances(..)
  , GenesisData(..)
  , GenesisDelegation(..)
  , GenesisHash(..)
  , GenesisInitializer(..)
  , GenesisSpec(..)
  , PoorSecret
  , TestnetBalanceOptions(..)
  , gsSecretKeys
  , gsSecretKeysPoor
  , mkConfig
  )
import Cardano.Chain.ProtocolConstants (kEpochSlots, kSlotSecurityParam)
import Cardano.Chain.Slotting (EpochIndex(..), EpochSlots)
import Cardano.Chain.Update (ProtocolParameters(..), SoftforkRule(..))
import Cardano.Crypto (SecretKey)

import Test.Cardano.Crypto.Dummy (dummyProtocolMagic)


dummyConfig :: Config
dummyConfig = dummyConfigStartTime (UTCTime (ModifiedJulianDay 0) 0)

dummyConfigStartTime :: UTCTime -> Config
dummyConfigStartTime startTime =
  either (panic . show) identity $ mkConfig startTime dummyGenesisSpec

dummyK :: BlockCount
dummyK = 10

dummyEpochSlots :: EpochSlots
dummyEpochSlots = kEpochSlots dummyK

dummySlotSecurityParam :: EpochSlots
dummySlotSecurityParam = kSlotSecurityParam dummyK

dummyGeneratedSecrets :: GeneratedSecrets
dummyGeneratedSecrets =
  fromMaybe
      (panic
        "The impossible happened: GeneratedSecrets should be in dummyConfig"
      )
    $ configGeneratedSecrets dummyConfig

dummyGenesisSecretsRich :: [SecretKey]
dummyGenesisSecretsRich = gsRichSecrets dummyGeneratedSecrets

dummyGenesisSecretsPoor :: [PoorSecret]
dummyGenesisSecretsPoor = gsPoorSecrets dummyGeneratedSecrets

dummyGenesisSecretKeys :: [SecretKey]
dummyGenesisSecretKeys = gsSecretKeys dummyGeneratedSecrets

dummyGenesisSecretKeysRich :: [SecretKey]
dummyGenesisSecretKeysRich = gsRichSecrets dummyGeneratedSecrets

dummyGenesisSecretKeysPoor :: [SecretKey]
dummyGenesisSecretKeysPoor = gsSecretKeysPoor dummyGeneratedSecrets

dummyGenesisSpec :: GenesisSpec
dummyGenesisSpec = UnsafeGenesisSpec
  { gsAvvmDistr   = dummyGenesisAvvmBalances
  , gsHeavyDelegation = UnsafeGenesisDelegation mempty
  , gsProtocolParameters = dummyProtocolParameters
  , gsK           = dummyK
  , gsProtocolMagic = dummyProtocolMagic
  , gsInitializer = dummyGenesisInitializer
  }

dummyGenesisAvvmBalances :: GenesisAvvmBalances
dummyGenesisAvvmBalances = GenesisAvvmBalances mempty

dummyProtocolParameters :: ProtocolParameters
dummyProtocolParameters = ProtocolParameters
  { ppScriptVersion    = 0
  , ppSlotDuration     = 7000
  , ppMaxBlockSize     = 2000000
  , ppMaxHeaderSize    = 2000000
  , ppMaxTxSize        = 4096
  , ppMaxProposalSize  = 700
  , ppMpcThd           = mkKnownLovelacePortion @10000000000000
  , ppHeavyDelThd      = mkKnownLovelacePortion @5000000000000
  , ppUpdateVoteThd    = mkKnownLovelacePortion @1000000000000
  , ppUpdateProposalThd = mkKnownLovelacePortion @100000000000000
  , ppUpdateImplicit   = 10
  , ppSoftforkRule     = SoftforkRule
    (mkKnownLovelacePortion @900000000000000)
    (mkKnownLovelacePortion @600000000000000)
    (mkKnownLovelacePortion @50000000000000)
  , ppTxFeePolicy      = TxFeePolicyTxSizeLinear
    (TxSizeLinear (mkKnownLovelace @155381) (mkKnownLovelace @44))
  , ppUnlockStakeEpoch = EpochIndex maxBound
  }

dummyGenesisInitializer :: GenesisInitializer
dummyGenesisInitializer = GenesisInitializer
  { giTestBalance = TestnetBalanceOptions
    { tboPoors          = 12
    , tboRichmen        = 4
    , tboTotalBalance   = mkKnownLovelace @6000000000000000
    , tboRichmenShare   = mkKnownLovelacePortion @990000000000000
    , tboUseHDAddresses = True
    }
  , giFakeAvvmBalance = FakeAvvmOptions
    { faoCount      = 10
    , faoOneBalance = mkKnownLovelace @100000
    }
  , giAvvmBalanceFactor = mkKnownLovelacePortion @1000000000000000
  , giUseHeavyDlg = True
  , giSeed        = 0
  }

dummyGenesisData :: GenesisData
dummyGenesisData = configGenesisData dummyConfig

dummyGenesisDataStartTime :: UTCTime -> GenesisData
dummyGenesisDataStartTime = configGenesisData . dummyConfigStartTime

dummyGenesisHash :: GenesisHash
dummyGenesisHash = configGenesisHash dummyConfig
