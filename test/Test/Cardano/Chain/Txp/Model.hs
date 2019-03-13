{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Cardano.Chain.Txp.Model
  ( prop_commandUTXOW
  )
where

import Cardano.Prelude

import Text.Show
import Data.Coerce
import Data.IORef
import qualified Data.Map.Strict as M

import qualified Cardano.Chain.Txp as Concrete
import qualified Cardano.Chain.Txp.UTxO as Concrete.UTxO
import qualified Control.State.Transition as STS
import qualified Ledger.Core as Abstract
import qualified Ledger.UTxO as Abstract

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import qualified Test.Cardano.Chain.Elaboration.UTxO as E
import Test.Cardano.Crypto.Dummy


--------------------------------------------------------------------------------
-- UTXOW
--------------------------------------------------------------------------------

prop_commandUTXOW :: Property
prop_commandUTXOW =
  withTests 50
    . property
    $ do
        concreteRef <- liftIO $ newIORef initialConcreteState

        let env = Abstract.ProtocolConstants (\_ -> Abstract.Value 100)

        actions     <- forAll $ Gen.sequential
          (Range.linear 1 100)
          initialStateUTXOW
          [commandUTXOW concreteRef env]

        cleanup concreteRef >> executeSequential initialStateUTXOW actions
 where
  initialConcreteState = Concrete.UTxO.empty

  cleanup :: IORef Concrete.UTxO -> PropertyT IO ()
  cleanup = liftIO . flip writeIORef initialConcreteState

type UTXOW = Abstract.UTXOW (Var Concrete.TxId Symbolic)

data StateUTXOW (v :: Type -> Type) = StateUTXOW
  { abstractState :: E.UTxO v
  , lastResult    :: Either [STS.PredicateFailure UTXOW] (E.UTxO v)
  } deriving (Eq, Show)

initialStateUTXOW :: StateUTXOW v
initialStateUTXOW = StateUTXOW
  { abstractState = initialUTxO
  , lastResult    = Right initialUTxO
  }
 where
  initialUTxO =
    Abstract.UTxO $ M.singleton (Abstract.TxIn _ _) (Abstract.TxOut _ _)


newtype SignalUTXOW (v :: Type -> Type)
  = SignalUTXOW (STS.Signal UTXOW)
  deriving (Show)

instance HTraversable SignalUTXOW where
  htraverse _ s = pure (coerce s)


commandUTXOW
  :: forall m
   . MonadIO m
  => IORef Concrete.UTxO
  -> STS.Environment UTXOW
  -> Command Gen m StateUTXOW
commandUTXOW concreteRef _env = Command gen execute callbacks
 where
  gen :: StateUTXOW v -> Maybe (Gen (SignalUTXOW v))
  gen StateUTXOW{} = Nothing -- Just $ SignalUTXOW <$> STS.sigGen @UTXOW env state

  execute
    :: SignalUTXOW Concrete
    -> m (Either Concrete.UTxOValidationError (Opaque Concrete.UTxO))
  execute (SignalUTXOW _transaction) = do
    concreteState <- liftIO $ readIORef concreteRef

    let
      result
        :: Either Concrete.UTxOValidationError (Opaque Concrete.UTxO)
      result = Opaque <$>
        Concrete.updateUTxOWitness dummyProtocolMagicId concreteState undefined

    liftIO . writeIORef concreteRef $ fromRight concreteState (unOpaque <$> result)

    pure result

  callbacks
    :: [ Callback
           SignalUTXOW
           (Either Concrete.UTxOValidationError (Opaque Concrete.UTxO))
           StateUTXOW
       ]
  callbacks = []

instance Show (Abstract.ProtocolConstants id) where
  show _ = "ProtocolConstants { pcMinFee = <function> }"
