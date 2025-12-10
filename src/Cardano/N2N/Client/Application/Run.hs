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
import Cardano.N2N.Client.Application.Options
    ( Limit (..)
    , Options (..)
    , optionsParser
    )
import Cardano.N2N.Client.Ouroboros.Connection (runNodeApplication)
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
import OptEnvConf (runParser)
import Paths_cardano_n2n_client (version)

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
        events <- newTBQueueIO 100
        doneVar <- newTVarIO False

        -- A TVar to count the number of synced blocks
        countVar <- newTVarIO $ Limit 0
        let useBlock _ = atomically $ do
                modifyTVar countVar succ
                count <- readTVar countVar
                when (count >= limit)
                    $ writeTVar doneVar True

        r <-
            runNodeApplication
                networkMagic
                nodeName
                portNumber
                (mkChainSyncApplication events doneVar startingPoint)
                (mkBlockFetchApplication events doneVar useBlock)
        case r of
            Left err -> throwIO err
            Right _ -> readTVarIO countVar
