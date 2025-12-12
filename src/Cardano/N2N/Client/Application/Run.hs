module Cardano.N2N.Client.Application.Run
    ( main
    )
where

import Cardano.N2N.Client.Application.BlockFetch
    ( mkBlockFetchApplication
    )
import Cardano.N2N.Client.Application.ChainSync
    ( mkChainSyncApplication
    )
import Cardano.N2N.Client.Application.Metrics
    ( MetricsEvent (..)
    , metricsTracer
    )
import Cardano.N2N.Client.Application.Options
    ( Limit (..)
    , Options (..)
    , optionsParser
    )
import Cardano.N2N.Client.Application.UTxOs (uTxOs)
import Cardano.N2N.Client.Ouroboros.Connection (runNodeApplication)
import Cardano.N2N.Client.Ouroboros.Types
    ( Block
    )
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , modifyTVar
    , newTBQueueIO
    , newTVarIO
    , readTVar
    , readTVarIO
    , writeTVar
    )
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Tracer (Contravariant (..), traceWith)
import OptEnvConf (runParser)
import Paths_cardano_n2n_client (version)
import System.IO
    ( BufferMode (..)
    , hSetBuffering
    , stdout
    )

main :: IO ()
main = do
    options <- runParser version "N2N app example" optionsParser
    e <- application options
    putStrLn $ "Synced " ++ show e ++ " blocks."

-- | Run an cardano-n2n-client application that connects to a node and syncs
-- blocks starting from the given point, up to the given limit.
application
    :: Options
    -- ^ limit of blocks to sync
    -> IO Limit
application
    Options
        { networkMagic
        , nodeName
        , portNumber
        , startingPoint
        , limit
        } = do
        hSetBuffering stdout NoBuffering
        events <- newTBQueueIO 100
        doneVar <- newTVarIO False

        tracer <- metricsTracer 1000

        -- A TVar to count the number of synced blocks
        countVar <- newTVarIO 0
        let useBlock block = do
                count <- atomically $ do
                    modifyTVar countVar succ
                    count <- readTVar countVar
                    when (Limit count >= limit)
                        $ writeTVar doneVar True
                    pure count
                traceWith tracer (BlockHeightMetrics count)
                printUTxOs block
        r <-
            runNodeApplication
                networkMagic
                nodeName
                portNumber
                (mkChainSyncApplication events doneVar startingPoint)
                ( mkBlockFetchApplication
                    (contramap BlockFetchMetrics tracer)
                    events
                    doneVar
                    useBlock
                )
        case r of
            Left err -> throwIO err
            Right _ -> Limit <$> readTVarIO countVar

printUTxOs :: Block -> IO ()
printUTxOs blk = print $ uTxOs blk
