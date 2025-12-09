module Cardano.N2N.Client.Application.BlockFetch
    ( mkBlockFetchApplication
    , BlockFetchApplication
    )
where

import Cardano.N2N.Client.Ouroboros.Types
    ( BlockFetchApplication
    , Header
    )
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , StrictTQueue
    , StrictTVar
    , modifyTVar
    , readTQueue
    )
import Data.Function (fix)
import Data.Functor (($>))
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Network.Block (blockPoint)
import Ouroboros.Network.BlockFetch.ClientState (ChainRange (..))
import Ouroboros.Network.Protocol.BlockFetch.Client
    ( BlockFetchClient (..)
    , BlockFetchReceiver (..)
    , BlockFetchRequest (..)
    , BlockFetchResponse (..)
    )

mkBlockFetchApplication
    :: StrictTQueue IO Header -> StrictTVar IO Int -> BlockFetchApplication
mkBlockFetchApplication blockReq count = BlockFetchClient $ do
    let addOne = atomically (modifyTVar count $ \(!n) -> n + 1)
    fix $ \fetch -> do
        point <- fmap blockPoint $ atomically $ readTQueue blockReq
        pure
            $ SendMsgRequestRange
                (ChainRange point point)
                BlockFetchResponse
                    { handleStartBatch = pure $ fix $ \fetchOne ->
                        BlockFetchReceiver
                            { handleBlock = \_ -> addOne $> fetchOne
                            , handleBatchDone = pure ()
                            }
                    , handleNoBlocks = pure ()
                    }
                BlockFetchClient{runBlockFetchClient = fetch}
