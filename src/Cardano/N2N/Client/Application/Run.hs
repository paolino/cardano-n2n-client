module Cardano.N2N.Client.Application.Run
    ( main
    )
where

import Cardano.N2N.Client.Application.ChainSync
    ( Limit (..)
    , mkChainSyncApplication
    )
import Cardano.N2N.Client.Ouroboros.Connection
    ( runNodeApplication
    )
import Cardano.N2N.Client.Ouroboros.Types (Header, Point)
import Control.Concurrent.Class.MonadSTM.Strict
    ( newTVarIO
    , readTVarIO
    )
import Control.Exception (SomeException, try)
import Network.Socket (PortNumber)
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Mock.Chain (Chain)
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.Point (WithOrigin (..))

-- | Run an cardano-n2n-client application that connects to a node and syncs
-- blocks starting from the given point, up to the given limit.
application
    :: NetworkMagic
    -- ^ network magic
    -> String
    -- ^ peer host
    -> PortNumber
    -- ^ peer port
    -> Point
    -- ^ starting point
    -> Limit
    -- ^ limit of blocks to sync
    -> IO (Either SomeException (Chain.Point Header))
application magic peerName peerPort startingPoint limit = do
    chainvar <- newTVarIO (Chain.Genesis :: Chain Header)
    res <-
        -- To gracefully handle the node getting killed it seems we need
        -- the outer 'try', even if connectToNode already returns 'Either
        -- SomeException'.
        try
            $ runNodeApplication
                magic
                peerName
                peerPort
                (mkChainSyncApplication chainvar startingPoint limit)
    case res of
        Left e -> return $ Left e
        Right _ -> pure . Chain.headPoint <$> readTVarIO chainvar

originPoint :: Point
originPoint = Network.Point Origin

preprod :: NetworkMagic
preprod = NetworkMagic 1

main :: IO ()
main = do
    e <-
        application
            preprod
            "cardano-node-preprod"
            3000
            originPoint
            (Limit 100)
    case e of
        Left err -> putStrLn $ "Error: " ++ show err
        Right point -> putStrLn $ "Synced up to point: " ++ show point
