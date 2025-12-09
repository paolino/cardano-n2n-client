module Cardano.N2N.Client.Application.ChainSync
    ( Limit (..)
    , mkChainSyncApplication
    )
where

import Cardano.N2N.Client.Ouroboros.Types
    ( ChainSyncApplication
    , Header
    , Point
    , Tip
    )
import Control.Concurrent.Class.MonadSTM.Strict
    ( MonadSTM (..)
    , StrictTQueue
    , StrictTVar
    , modifyTVar
    , readTVar
    , writeTQueue
    )
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Network.Mock.Chain (Chain)
import Ouroboros.Network.Mock.Chain qualified as Chain
import Ouroboros.Network.NodeToNode
    ( ControlMessage (..)
    , ControlMessageSTM
    )
import Ouroboros.Network.Protocol.ChainSync.Client
    ( ChainSyncClient (..)
    , ClientStIdle (..)
    , ClientStIntersect (..)
    , ClientStNext (..)
    )

type State = StrictTVar IO (Chain Header)

rollForward :: StrictTQueue IO Header -> State -> Header -> IO ()
rollForward blockReq chainvar b = atomically $ do
    modifyTVar chainvar $ \(!chain) -> Chain.addBlock b chain
    writeTQueue blockReq b

-- We will fail to roll back iff `p` doesn't exist in `chain`
-- This will happen when we're asked to roll back to `startingPoint`,
-- which we can check for, or any point before, which we can't
-- check for. Hence we ignore all failures to rollback and replace the
-- chain with an empty one if we do.
rollBackward :: State -> Point -> IO ()
rollBackward chainvar p = atomically $ modifyTVar chainvar $ \(!chain) ->
    fromMaybe Chain.Genesis $ Chain.rollback p chain

-- | A limit on the number of blocks to sync
newtype Limit = Limit {limit :: Word32}
    deriving newtype (Show, Read, Eq, Ord)

-- the way to step the protocol
type StepProtocol = IO (Maybe Protocol)

-- Internal protocol state machine
data Protocol = Protocol
    { onRollBackward :: Point -> Tip -> StepProtocol
    , onRollForward :: Header -> StepProtocol
    , points :: [Point] -> StepProtocol
    }

-- A protocol controlled by control messages
controlledProtocol
    :: ControlMessageSTM IO -- control message source
    -> Protocol
controlledProtocol controlMessageSTM = fix $ \client ->
    let react ctrl = case ctrl of
            Continue -> Just client
            Quiesce -> error "controlledClient: unexpected Quiesce"
            Terminate -> Nothing
    in  Protocol
            { onRollBackward = \_point _tip ->
                react <$> atomically controlMessageSTM
            , onRollForward = \_header ->
                react <$> atomically controlMessageSTM
            , points = \_points -> pure $ Just client
            }

-- a control message source that terminates after syncing 'limit' blocks
terminateAfterCount
    :: State
    -> Limit
    -> ControlMessageSTM IO
terminateAfterCount chainvar limit = do
    chainLength <-
        Limit . fromIntegral . Chain.length <$> readTVar chainvar
    pure
        $ if chainLength < limit
            then Continue
            else Terminate

-- initialize the protocol with a control message source
mkProtocol
    :: State -- the mock chain
    -> Limit -- limit of blocks to sync
    -> Protocol
mkProtocol stateVar limit =
    controlledProtocol $ terminateAfterCount stateVar limit

-- The idle state of the chain sync client
type ChainSyncIdle = ClientStIdle Header Point Tip IO ()

-- when the protocols returns Nothing, we're done as a N2N client
nothingToDone
    :: Maybe Protocol
    -> (Protocol -> ChainSyncIdle)
    -> ChainSyncIdle
nothingToDone Nothing _ = SendMsgDone ()
nothingToDone (Just next) cont = cont next

-- boots the protocol and step into initialise
mkChainSyncApplication
    :: StrictTQueue IO Header
    -> State
    -- ^ the mock chain
    -> Point
    -- ^ starting point
    -> Limit
    -- ^ limit of blocks to sync
    -> ChainSyncApplication
    -- ^ the chain sync client application
mkChainSyncApplication blockReq stateVar startingPoint limit = ChainSyncClient $ do
    ps <- points (mkProtocol stateVar limit) [startingPoint]
    pure $ nothingToDone ps $ initialise blockReq stateVar startingPoint

-- In this consumer example, we do not care about whether the server
-- found an intersection or not. If not, we'll just sync from genesis.
--
-- Alternative policies here include:
--  iteratively finding the best intersection
--  rejecting the server if there is no intersection in the last K blocks
--
initialise
    :: StrictTQueue IO Header
    -> State -- the mock chain
    -> Point -- starting point
    -> Protocol -- previous client state machine
    -> ChainSyncIdle
initialise blockReq stateVar startingPoint prev =
    let next =
            ChainSyncClient
                { runChainSyncClient = pure $ requestNext blockReq stateVar prev
                }
    in  SendMsgFindIntersect [startingPoint]
            $ ClientStIntersect
                { recvMsgIntersectFound = \_point _tip -> next
                , recvMsgIntersectNotFound = \_tip -> next
                }

requestNext
    :: StrictTQueue IO Header
    -> State -- the mock chain
    -> Protocol -- this client state machine
    -> ChainSyncIdle
requestNext blockReq stateVar prev =
    SendMsgRequestNext
        -- We have the opportunity to do something when receiving
        -- MsgAwaitReply. In this example we don't take up that opportunity.
        (pure ())
        ClientStNext
            { recvMsgRollForward = \header _tip -> ChainSyncClient $ do
                rollForward blockReq stateVar header
                choice <- onRollForward prev header
                pure $ nothingToDone choice $ requestNext blockReq stateVar
            , recvMsgRollBackward = \pIntersect tip -> ChainSyncClient $ do
                rollBackward stateVar pIntersect
                choice <- onRollBackward prev pIntersect tip
                pure $ nothingToDone choice $ requestNext blockReq stateVar
            }
