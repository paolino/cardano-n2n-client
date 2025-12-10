{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Cardano.Read.Ledger.Tx.Hash
    ( byronTxHash
    , shelleyTxHash
    , fromShelleyTxId
    , getEraTxHash
    )
where

import Prelude

import Cardano.Chain.UTxO
    ( ATxAux
    , taTx
    )
import Cardano.Crypto
    ( serializeCborHash
    )
import Cardano.Crypto qualified as CryptoC
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Core
    ( bodyTxL
    , txIdTxBody
    )
import Cardano.Ledger.Core qualified as SL.Core
import Cardano.Ledger.Hashes qualified as SafeHash
import Cardano.Ledger.TxIn
    ( TxId (..)
    )
import Cardano.Read.Ledger.Eras
    ( Era (..)
    , IsEra (..)
    )
import Cardano.Read.Ledger.Tx.Eras
    ( onTx
    )
import Cardano.Read.Ledger.Tx.Tx
    ( Tx
    )
import Control.Lens
    ( (^.)
    )

{-# INLINE getEraTxHash #-}

-- | Extract the hash of a transaction in any era.
getEraTxHash :: forall era. IsEra era => Tx era -> Crypto.ByteString
getEraTxHash = case theEra @era of
    Byron -> onTx byronTxHash
    Shelley -> mkShelleyHash
    Allegra -> mkShelleyHash
    Mary -> mkShelleyHash
    Alonzo -> mkShelleyHash
    Babbage -> mkShelleyHash
    Conway -> mkShelleyHash
  where
    mkShelleyHash = onTx $ \tx -> shelleyTxHash tx

shelleyTxHash
    :: SL.Core.EraTx era => SL.Core.Tx era -> Crypto.ByteString
shelleyTxHash tx = fromShelleyTxId $ txIdTxBody (tx ^. bodyTxL)

byronTxHash :: ATxAux a -> Crypto.ByteString
byronTxHash = CryptoC.hashToBytes . serializeCborHash . taTx

fromShelleyTxId :: TxId -> Crypto.ByteString
fromShelleyTxId (TxId h) =
    Crypto.hashToBytes $ SafeHash.extractHash h
