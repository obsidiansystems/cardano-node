-- | Node client support for the Voltaire protocol
--
module Cardano.Api.Protocol.Voltaire
  ( -- * Client support
    mkNodeClientProtocolVoltaire
  , mkSomeNodeClientProtocolVoltaire
  ) where

import Cardano.Api.Protocol.Types
    ( SomeNodeClientProtocol(..),
      ProtocolClient(ProtocolClientInfoArgs),
      ProtocolClientInfoArgs(ProtocolClientInfoArgsVoltaire) )
import qualified Ouroboros.Consensus.Voltaire.Prototype.Block as Voltaire
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

mkNodeClientProtocolVoltaire :: ProtocolClientInfoArgs (Voltaire.VoltairePrototypeBlock StandardCrypto)
mkNodeClientProtocolVoltaire = ProtocolClientInfoArgsVoltaire

mkSomeNodeClientProtocolVoltaire :: SomeNodeClientProtocol
mkSomeNodeClientProtocolVoltaire =
    SomeNodeClientProtocol
      mkNodeClientProtocolVoltaire
