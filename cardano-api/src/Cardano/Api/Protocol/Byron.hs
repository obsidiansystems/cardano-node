-- | Node client support for the Byron protocol
--
module Cardano.Api.Protocol.Byron
  ( -- * Client support
    mkNodeClientProtocolByron
  , mkSomeNodeClientProtocolByron
  ) where

import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol (..))
import           Cardano.Chain.Slotting (EpochSlots)
import           Ouroboros.Consensus.Cardano (ProtocolClient(..), ProtocolByron,
                     RunProtocolClient(RunProtocolClientByron))
import           Ouroboros.Consensus.Cardano.ByronHFC

mkNodeClientProtocolByron :: EpochSlots
                          -> RunProtocolClient ByronBlockHFC ProtocolByron
mkNodeClientProtocolByron = RunProtocolClientByron

mkSomeNodeClientProtocolByron :: EpochSlots
                              -> SomeNodeClientProtocol
mkSomeNodeClientProtocolByron epochSlots =
    SomeNodeClientProtocol
      (mkNodeClientProtocolByron epochSlots)
