module Cardano.N2N.Client.Application.TxIns
    ( txIns
    )
where

import Cardano.Ledger.Binary (EncCBOR, natVersion, serialize)
import Cardano.N2N.Client.Ouroboros.Types (Block)
import Cardano.Read.Ledger.Block.Block (fromConsensusBlock)
import Cardano.Read.Ledger.Block.Txs (getEraTransactions)
import Cardano.Read.Ledger.Eras.EraValue (applyEraFun)
import Cardano.Read.Ledger.Eras.KnownEras (Era (..), IsEra, theEra)
import Cardano.Read.Ledger.Tx.Inputs (Inputs (..), getEraInputs)
import Cardano.Read.Ledger.Tx.Tx (Tx (..))
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (toList)

txIns :: Block -> [ByteString]
txIns bl =
    (txInsOfTx . getEraTransactions) `applyEraFun` fromConsensusBlock bl

txInsOfTx :: IsEra era => [Tx era] -> [ByteString]
txInsOfTx = concatMap (extractInputs . getEraInputs)

extractInputs :: forall era. IsEra era => Inputs era -> [ByteString]
extractInputs (Inputs ins) = case theEra @era of
    Byron -> cborEncode <$> toList ins
    Shelley -> cborEncode <$> toList ins
    Allegra -> cborEncode <$> toList ins
    Mary -> cborEncode <$> toList ins
    Alonzo -> cborEncode <$> toList ins
    Babbage -> cborEncode <$> toList ins
    Conway -> cborEncode <$> toList ins

cborEncode :: EncCBOR a => a -> ByteString
cborEncode = serialize (natVersion @11)

_txs :: forall era. IsEra era => Tx era -> ByteString
_txs (Tx tx) = case theEra @era of
    Byron -> cborEncode tx
    Shelley -> cborEncode tx
    Allegra -> cborEncode tx
    Mary -> cborEncode tx
    Alonzo -> cborEncode tx
    Babbage -> cborEncode tx
    Conway -> cborEncode tx
