module Cardano.N2N.Client.Application.BlockFetch
    ( mkBlockFetchApplication
    , BlockFetchApplication
    )
where

import Cardano.N2N.Client.Application.ChainSync (Event (..))
import Cardano.N2N.Client.Ouroboros.Types
    ( Block
    , BlockFetchApplication
    , Point
    )
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , StrictTBQueue
    , StrictTVar
    , flushTBQueue
    , readTVarIO
    )
import Data.Function (fix)
import Data.Functor (($>))
import Data.Maybe (mapMaybe)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Network.Block (blockPoint)
import Ouroboros.Network.BlockFetch.ClientState (ChainRange (..))
import Ouroboros.Network.Protocol.BlockFetch.Client
    ( BlockFetchClient (..)
    , BlockFetchReceiver (..)
    , BlockFetchRequest (..)
    , BlockFetchResponse (..)
    )
import System.IO (hPrint, stderr)

nextChainRange
    :: MonadSTM m => StrictTBQueue m Event -> STM m (ChainRange Point)
nextChainRange events = do
    xs <- mapMaybe getPoint <$> flushTBQueue events
    case xs of
        [] -> retry
        y : ys -> pure . ChainRange y $ case ys of
            [] -> y
            _ -> last ys
  where
    getPoint (RollForward header _) = Just $ blockPoint header
    getPoint (RollBackward _point _) = Nothing

mkBlockFetchApplication
    :: StrictTBQueue IO Event
    -- ^ queue of headers to request blocks for
    -> StrictTVar IO Bool
    -- ^ variable indicating whether we're done
    -> (Block -> IO ())
    -- ^ callback to process each fetched block
    -> BlockFetchApplication
mkBlockFetchApplication events doneVar cb = BlockFetchClient
    $ fix
    $ \fetch -> do
        done <- readTVarIO doneVar
        if done
            then pure $ SendMsgClientDone ()
            else do
                points <- atomically $ nextChainRange events
                hPrint stderr points
                pure
                    $ SendMsgRequestRange
                        points
                        BlockFetchResponse
                            { handleStartBatch = pure $ fix $ \fetchOne ->
                                BlockFetchReceiver
                                    { handleBlock = \block ->
                                        cb block $> fetchOne
                                    , handleBatchDone = pure ()
                                    }
                            , handleNoBlocks = pure ()
                            }
                        BlockFetchClient{runBlockFetchClient = fetch}
