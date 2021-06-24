-- | Node client support for the Cardano protocol
--
module Cardano.Api.Protocol.Voltaire.Prototype
  ( -- * Client support
    mkNodeClientProtocolVoltairePrototype
  , mkSomeNodeClientProtocolVoltairePrototype
  ) where

import           Cardano.Api.Protocol.Types (ProtocolClient(..),
                     ProtocolClientInfoArgs(ProtocolClientInfoArgsVoltairePrototype),
                     SomeNodeClientProtocol (..))
import           Ouroboros.Consensus.Voltaire.Prototype.Block (VoltairePrototypeBlock)
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

mkNodeClientProtocolVoltairePrototype :: ProtocolClientInfoArgs (VoltairePrototypeBlock StandardCrypto)
mkNodeClientProtocolVoltairePrototype = ProtocolClientInfoArgsVoltairePrototype

mkSomeNodeClientProtocolVoltairePrototype :: SomeNodeClientProtocol
mkSomeNodeClientProtocolVoltairePrototype =
    SomeNodeClientProtocol
      mkNodeClientProtocolVoltairePrototype
