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
    ( Options (..)
    , optionsParser
    )
import Cardano.N2N.Client.Ouroboros.Connection (runNodeApplication)
import Control.Concurrent.Class.MonadSTM.Strict
    ( newTQueueIO
    , newTVarIO
    , readTVarIO
    )
import Control.Exception (throwIO)
import OptEnvConf (runParser)
import Ouroboros.Network.Mock.Chain qualified as Chain
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
    -> IO Int
application
    Options
        { networkMagic
        , nodeName
        , portNumber
        , startingPoint
        , limit
        } = do
        chainvar <- newTVarIO Chain.Genesis
        blockReq <- newTQueueIO
        count <- newTVarIO (0 :: Int)
        r <-
            runNodeApplication
                networkMagic
                nodeName
                portNumber
                (mkChainSyncApplication blockReq chainvar startingPoint limit)
                (mkBlockFetchApplication blockReq count)
        case r of
            Left err -> throwIO err
            Right _ -> readTVarIO count
