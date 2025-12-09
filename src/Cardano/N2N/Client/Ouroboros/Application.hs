module Cardano.N2N.Client.Ouroboros.Application
    ( mkOuroborosApplication
    )
where

import Cardano.N2N.Client.Ouroboros.Codecs (codecChainSync)
import Cardano.N2N.Client.Ouroboros.Types (ChainSyncApplication)
import Control.Tracer (Contravariant (contramap), stdoutTracer)
import Data.ByteString.Lazy (LazyByteString)
import Data.Void (Void)
import Network.Mux qualified as Mx
import Ouroboros.Consensus.Protocol.Praos.Header ()
import Ouroboros.Consensus.Shelley.Ledger.NetworkProtocolVersion ()
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.Mux
    ( MiniProtocol (..)
    , MiniProtocolLimits (..)
    , MiniProtocolNum (MiniProtocolNum)
    , OuroborosApplication (..)
    , OuroborosApplicationWithMinimalCtx
    , RunMiniProtocol (InitiatorProtocolOnly)
    , StartOnDemandOrEagerly (StartOnDemand)
    , mkMiniProtocolCbFromPeer
    )
import Ouroboros.Network.Protocol.ChainSync.Client qualified as ChainSync

-- TODO: provide sensible limits
-- https://github.com/intersectmbo/ouroboros-network/issues/575
maximumMiniProtocolLimits :: MiniProtocolLimits
maximumMiniProtocolLimits =
    MiniProtocolLimits
        { maximumIngressQueue = maxBound
        }

mkOuroborosApplication
    :: ChainSyncApplication
    -- ^ chainSync
    -> OuroborosApplicationWithMinimalCtx
        Mx.InitiatorMode
        addr
        LazyByteString
        IO
        ()
        Void
mkOuroborosApplication chainSyncApp =
    OuroborosApplication
        { getOuroborosApplication =
            [ MiniProtocol
                { miniProtocolNum = MiniProtocolNum 2
                , miniProtocolStart = StartOnDemand
                , miniProtocolLimits = maximumMiniProtocolLimits
                , miniProtocolRun = run
                }
            ]
        }
  where
    run = InitiatorProtocolOnly
        $ mkMiniProtocolCbFromPeer
        $ \_ctx ->
            ( contramap show stdoutTracer -- tracer
            , codecChainSync
            , ChainSync.chainSyncClientPeer chainSyncApp
            )
