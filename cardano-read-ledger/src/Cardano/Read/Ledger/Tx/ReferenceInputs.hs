{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2020-2022 IOHK
-- License: Apache-2.0
--
-- Reference input data extraction from 'Tx'
module Cardano.Read.Ledger.Tx.ReferenceInputs
    ( ReferenceInputsType
    , ReferenceInputs (..)
    , getEraReferenceInputs
    )
where

import Prelude

import Cardano.Ledger.Babbage.TxBody
    ( referenceInputsTxBodyL
    )
import Cardano.Ledger.Core
    ( bodyTxL
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
import Data.Set
    ( Set
    )

type family ReferenceInputsType era where
    ReferenceInputsType Byron = ()
    ReferenceInputsType Shelley = ()
    ReferenceInputsType Allegra = ()
    ReferenceInputsType Mary = ()
    ReferenceInputsType Alonzo = ()
    ReferenceInputsType Babbage = Set SH.TxIn
    ReferenceInputsType Conway = Set SH.TxIn

newtype ReferenceInputs era = ReferenceInputs (ReferenceInputsType era)

deriving instance
    Show (ReferenceInputsType era) => Show (ReferenceInputs era)
deriving instance
    Eq (ReferenceInputsType era) => Eq (ReferenceInputs era)

{-# INLINEABLE getEraReferenceInputs #-}
getEraReferenceInputs
    :: forall era. IsEra era => Tx era -> ReferenceInputs era
getEraReferenceInputs = case theEra @era of
    Byron -> \_ -> ReferenceInputs ()
    Shelley -> \_ -> ReferenceInputs ()
    Allegra -> \_ -> ReferenceInputs ()
    Mary -> \_ -> ReferenceInputs ()
    Alonzo -> \_ -> ReferenceInputs ()
    Babbage -> referenceInputsBabbage
    Conway -> referenceInputsBabbage
  where
    referenceInputsBabbage = onTx $ \tx ->
        ReferenceInputs $ tx ^. bodyTxL . referenceInputsTxBodyL
