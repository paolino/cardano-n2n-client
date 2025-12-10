module Cardano.N2N.Client.Application.Options
    ( Options (..)
    , Limit (..)
    , networkMagicOption
    , nodeNameOption
    , portNumberOption
    , limitOption
    , optionsParser
    )
where

import Cardano.N2N.Client.Ouroboros.Types (Point)
import Data.Word (Word32)
import Network.Socket (PortNumber)
import OptEnvConf
    ( Parser
    , auto
    , help
    , long
    , metavar
    , option
    , reader
    , setting
    , str
    , strOption
    , value
    )
import Ouroboros.Network.Block qualified as Network
import Ouroboros.Network.Magic (NetworkMagic (..))
import Ouroboros.Network.Point (WithOrigin (..))

-- | A limit on the number of blocks to sync
newtype Limit = Limit {limit :: Word32}
    deriving newtype (Show, Read, Eq, Ord, Enum)

data Options = Options
    { networkMagic :: NetworkMagic
    , nodeName :: String
    , portNumber :: PortNumber
    , startingPoint :: Point
    , limit :: Limit
    }

networkMagicOption :: Parser NetworkMagic
networkMagicOption =
    NetworkMagic
        <$> setting
            [ long "network-magic"
            , help "Network magic number"
            , metavar "INT"
            , value 1
            , reader auto
            , option
            ]

nodeNameOption :: Parser String
nodeNameOption =
    strOption
        [ long "node-name"
        , help "Peer node hostname"
        , metavar "HOSTNAME"
        , value "preprod-node.play.dev.cardano.org"
        , reader str
        , option
        ]

portNumberOption :: Parser PortNumber
portNumberOption =
    setting
        [ long "port"
        , help "Peer node port number"
        , metavar "INT"
        , value 3001
        , reader auto
        , option
        ]

limitOption :: Parser Limit
limitOption =
    Limit
        <$> setting
            [ long "limit"
            , help "Limit on number of blocks to sync"
            , metavar "INT"
            , value 10
            , reader auto
            , option
            ]

-- startingPointOption :: Parser Point
-- startingPointOption = Network.Point <$> setting
--     [ long "starting-point"
--     , help "Starting point to sync from (format: origin or slot number)"
--     , metavar "POINT"
--     , value Origin
--     , reader parsePoint
--     ]

optionsParser :: Parser Options
optionsParser =
    Options
        <$> networkMagicOption
        <*> nodeNameOption
        <*> portNumberOption
        <*> pure (Network.Point Origin)
        <*> limitOption
