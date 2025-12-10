module Cardano.N2N.Client.Application.TxIns
    ( txIns
    )
where

import Cardano.Ledger.Api.Tx.In (TxIn)
import Cardano.N2N.Client.Ouroboros.Types (Block)

txIns :: Block -> [TxIn]
txIns = undefined
