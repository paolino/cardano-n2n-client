module Cardano.N2N.Client.Application.KeepAlive
    ( keepAliveApplication
    ) where

import Cardano.N2N.Client.Ouroboros.Types (KeepAliveApplication)
import Data.Function (fix)
import Ouroboros.Network.Protocol.KeepAlive.Client
    ( KeepAliveClient (..)
    , KeepAliveClientSt (SendMsgKeepAlive)
    )
import Ouroboros.Network.Protocol.KeepAlive.Type (Cookie (..))

keepAliveApplication :: KeepAliveApplication
keepAliveApplication =
    KeepAliveClient
        $ fix
        $ pure . SendMsgKeepAlive Cookie{unCookie = 0}
