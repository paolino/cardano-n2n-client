{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2024 Cardano Foundation
-- License: Apache-2.0
--
-- Era-indexed value.
module Cardano.Read.Ledger.Value
    ( ValueType
    , Value (..)
    , maryValueFromByronValue
    , maryValueFromShelleyValue
    )
where

import Prelude

import Cardano.Chain.Common qualified as BY
import Cardano.Ledger.BaseTypes qualified as SH
import Cardano.Ledger.Coin qualified as SH
import Cardano.Ledger.Mary.Value qualified as MA
import Cardano.Read.Ledger.Eras
    ( Allegra
    , Alonzo
    , Babbage
    , Byron
    , Conway
    , Mary
    , Shelley
    )

{-----------------------------------------------------------------------------
    Value
------------------------------------------------------------------------------}

type family ValueType era where
    ValueType Byron = BY.Lovelace
    ValueType Shelley = SH.Coin
    ValueType Allegra = SH.Coin
    ValueType Mary = MA.MaryValue
    ValueType Alonzo = MA.MaryValue
    ValueType Babbage = MA.MaryValue
    ValueType Conway = MA.MaryValue

newtype Value era = Value (ValueType era)

deriving instance Show (ValueType era) => Show (Value era)
deriving instance Eq (ValueType era) => Eq (Value era)

maryValueFromByronValue :: ValueType Byron -> ValueType Mary
maryValueFromByronValue = SH.inject . SH.Coin . BY.lovelaceToInteger

maryValueFromShelleyValue :: ValueType Shelley -> ValueType Mary
maryValueFromShelleyValue = SH.inject
