-- | Node client support for the Shelley protocol
--
module Cardano.Api.Protocol.Shelley
  ( -- * Client support
    mkNodeClientProtocolShelley
  , mkSomeNodeClientProtocolShelley
  ) where


import           Ouroboros.Consensus.Cardano (ProtocolClient(..),
                     ProtocolShelley, RunProtocolClient(..))
import           Ouroboros.Consensus.Cardano.ShelleyHFC

import           Ouroboros.Consensus.Shelley.Eras (StandardShelley)

import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol (..))


mkNodeClientProtocolShelley :: RunProtocolClient
                                 (ShelleyBlockHFC StandardShelley)
                                 ProtocolShelley
mkNodeClientProtocolShelley = RunProtocolClientShelley


mkSomeNodeClientProtocolShelley :: SomeNodeClientProtocol
mkSomeNodeClientProtocolShelley =
    SomeNodeClientProtocol mkNodeClientProtocolShelley
