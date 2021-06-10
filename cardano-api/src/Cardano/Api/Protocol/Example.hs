-- | Node client support for the Cardano protocol
--
module Cardano.Api.Protocol.Example
  ( -- * Client support
    mkNodeClientProtocolExample
  , mkSomeNodeClientProtocolExample
  ) where

import           Cardano.Api.Protocol.Types (ProtocolClient(..),
                     ProtocolClientInfoArgs(ProtocolClientInfoArgsExample),
                     SomeNodeClientProtocol (..))
import           Ouroboros.Consensus.Example.Block (ExampleBlock)
import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

mkNodeClientProtocolExample :: ProtocolClientInfoArgs (ExampleBlock StandardCrypto)
mkNodeClientProtocolExample = ProtocolClientInfoArgsExample

mkSomeNodeClientProtocolExample :: SomeNodeClientProtocol
mkSomeNodeClientProtocolExample =
    SomeNodeClientProtocol
      mkNodeClientProtocolExample
