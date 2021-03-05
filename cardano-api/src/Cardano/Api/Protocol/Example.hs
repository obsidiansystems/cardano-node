-- | Node client support for the Cardano protocol
--
module Cardano.Api.Protocol.Example
  ( -- * Client support
    mkNodeClientProtocolExample
  , mkSomeNodeClientProtocolExample
  ) where

import           Cardano.Api.Protocol.Types (SomeNodeClientProtocol (..))
import           Ouroboros.Consensus.Cardano (ProtocolClient(..),
                     RunProtocolClient (..))
import           Ouroboros.Consensus.Example (ProtocolExample, RunProtocolClient(..))
import           Ouroboros.Consensus.Example.Block (ExampleBlock)
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

mkNodeClientProtocolExample :: RunProtocolClient (ExampleBlock StandardCrypto)
                                              ProtocolExample
mkNodeClientProtocolExample = RunProtocolClientExample

mkSomeNodeClientProtocolExample :: SomeNodeClientProtocol
mkSomeNodeClientProtocolExample =
    SomeNodeClientProtocol mkNodeClientProtocolExample
