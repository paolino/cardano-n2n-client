module Cardano.Read.Ledger.Block.BlockNo
    ( getEraBlockNo
    , BlockNo (..)
    , prettyBlockNo
    ) where

import Prelude

import Cardano.Read.Ledger.Block.BHeader
    ( BHeader (..)
    )
import Cardano.Read.Ledger.Eras
    ( Era (..)
    , IsEra (..)
    )
import GHC.Generics
    ( Generic
    )
import NoThunks.Class
    ( NoThunks (..)
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.Shelley.Protocol.Abstract
    ( pHeaderBlock
    )
import Ouroboros.Consensus.Shelley.Protocol.Praos
    (
    )
import Ouroboros.Consensus.Shelley.Protocol.TPraos
    (
    )

import Data.Text qualified as T
import Ouroboros.Network.Block qualified as O

{-# INLINEABLE getEraBlockNo #-}
getEraBlockNo :: forall era. IsEra era => BHeader era -> BlockNo
getEraBlockNo = case theEra @era of
    Byron -> \(BHeader h) -> k $ O.blockNo h
    Shelley -> \(BHeader h) -> k $ pHeaderBlock h
    Allegra -> \(BHeader h) -> k $ pHeaderBlock h
    Mary -> \(BHeader h) -> k $ pHeaderBlock h
    Alonzo -> \(BHeader h) -> k $ pHeaderBlock h
    Babbage -> \(BHeader h) -> k $ pHeaderBlock h
    Conway -> \(BHeader h) -> k $ pHeaderBlock h
  where
    k = BlockNo . fromIntegral . O.unBlockNo

newtype BlockNo = BlockNo {unBlockNo :: Natural}
    deriving (Eq, Ord, Show, Generic, Enum)

instance NoThunks BlockNo

-- | Short printed representation of a 'BlockNo'.
prettyBlockNo :: BlockNo -> T.Text
prettyBlockNo (BlockNo n) = T.pack (show n)
