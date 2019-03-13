{-# LANGUAGE OverloadedStrings #-}

module Test.Cardano.Chain.Elaboration.UTxO
  ( UTxO
  , Tx
  , TxIn
  , elaborateTransactionBS
  )
where

import Cardano.Prelude

import qualified Data.List.NonEmpty as NE
import qualified Data.Vector as V
import Formatting

-- import Cardano.Binary.Class
import qualified Cardano.Chain.Common as Concrete
import qualified Cardano.Chain.Txp as Concrete
import qualified Ledger.Core as Abstract
import qualified Ledger.UTxO as Abstract

import Hedgehog

import Test.Cardano.Chain.Elaboration.Keys


type UTxO v = Abstract.UTxO (Var Concrete.TxId v)

type Tx v = Abstract.Tx (Var Concrete.TxId v)

type TxIn v = Abstract.TxIn (Var Concrete.TxId v)


elaborateTransactionBS :: Tx Concrete -> Concrete.ATxAux ByteString
elaborateTransactionBS abstractTx = annotateTxAux
  $ elaborateTransaction abstractTx
  where annotateTxAux = undefined

elaborateTransaction :: Tx Concrete -> Concrete.TxAux
elaborateTransaction abstractTx = witnessTransaction concreteTransaction
 where
  concreteTransaction = Concrete.UnsafeTx
    { Concrete._txInputs     = elaborateTxInputs (Abstract.inputs abstractTx)
    , Concrete._txOutputs    = elaborateTxOutputs (Abstract.outputs abstractTx)
    , Concrete._txAttributes = Concrete.mkAttributes ()
    }

witnessTransaction :: Concrete.Tx -> Concrete.TxAux
witnessTransaction concreteTransaction = Concrete.mkTxAux
  concreteTransaction
  witness
 where
  witness =
    V.fromList
      .   toList
      $   witnessTxInput
      <$> Concrete._txInputs concreteTransaction

witnessTxInput :: Concrete.TxIn -> Concrete.TxInWitness
witnessTxInput concreteInput = undefined

elaborateTxInputs :: Set (TxIn Concrete) -> NonEmpty Concrete.TxIn
elaborateTxInputs abstractTxInputs = undefined

elaborateTxOutputs :: [Abstract.TxOut] -> NonEmpty Concrete.TxOut
elaborateTxOutputs =
  fromMaybe
      (panic
        "elaborateTxOuts: Tried to elaborate an empty list of Abstract.TxOuts"
      )
    . NE.nonEmpty
    . fmap elaborateTxOutput

elaborateTxOutput :: Abstract.TxOut -> Concrete.TxOut
elaborateTxOutput abstractTxOut = Concrete.TxOut
  { Concrete.txOutAddress = Concrete.makePubKeyAddress
    (elaborateVKey abstractPK)
  , Concrete.txOutValue   = lovelaceValue
  }
 where
  Abstract.TxOut (Abstract.Addr abstractPK) (Abstract.Value value) =
    abstractTxOut

  lovelaceValue = case Concrete.mkLovelace (fromIntegral value) of
    Left  err -> panic $ sformat build err
    Right l   -> l
