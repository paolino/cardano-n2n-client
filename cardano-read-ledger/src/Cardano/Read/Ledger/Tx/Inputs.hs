{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020-2024 IOHK
-- License: Apache-2.0
module Cardano.Read.Ledger.Tx.Inputs
    ( InputsType
    , Inputs (..)
    , getEraInputs
    )
where

import Prelude

import Cardano.Chain.UTxO qualified as BY
import Cardano.Ledger.Core
    ( bodyTxL
    , inputsTxBodyL
    )
import Cardano.Ledger.Shelley.API qualified as SH
import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Era (..)
    , IsEra (..)
    , Mary
    , Shelley
    )
import Cardano.Read.Ledger.Tx.Eras
    ( onTx
    )
import Cardano.Read.Ledger.Tx.Tx
    ( Tx (..)
    )
import Control.Lens
    ( (^.)
    )
import Data.List.NonEmpty
    ( NonEmpty
    )
import Data.Set
    ( Set
    )

type family InputsType era where
    InputsType Byron = NonEmpty BY.TxIn
    InputsType Shelley = Set SH.TxIn
    InputsType Allegra = Set SH.TxIn
    InputsType Mary = Set SH.TxIn
    InputsType Alonzo = Set SH.TxIn
    InputsType Babbage = Set SH.TxIn
    InputsType Conway = Set SH.TxIn
newtype Inputs era = Inputs (InputsType era)

deriving instance Show (InputsType era) => Show (Inputs era)
deriving instance Eq (InputsType era) => Eq (Inputs era)

{-# INLINEABLE getEraInputs #-}
getEraInputs :: forall era. IsEra era => Tx era -> Inputs era
getEraInputs = case theEra @era of
    Byron -> onTx $ \tx -> Inputs $ BY.txInputs $ BY.taTx tx
    Shelley -> shelleyInputs
    Allegra -> shelleyInputs
    Mary -> shelleyInputs
    Alonzo -> shelleyInputs
    Babbage -> shelleyInputs
    Conway -> shelleyInputs
  where
    shelleyInputs = onTx $ \tx -> Inputs (tx ^. bodyTxL . inputsTxBodyL)
