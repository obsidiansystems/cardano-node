module Cardano.Api.Update
( VoltaireUpdateClass(..)
)
where

import Data.Proxy (Proxy)
import Data.Map.Strict (Map)

import Cardano.Api.ProtocolParameters (ProtocolParametersUpdate)
import Ouroboros.Consensus.Shelley.Update (HasProtocolUpdates, ProposedProtocolUpdates)
import Cardano.Api.Hash (Hash)
import Cardano.Api.KeysShelley (GenesisKey)

class HasProtocolUpdates era => VoltaireUpdateClass era where
  toProtocolParametersUpdate
    :: Proxy era
    -> ProposedProtocolUpdates era
    -> Map (Hash GenesisKey) ProtocolParametersUpdate
