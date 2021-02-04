{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Protocol.Example
  (
    -- * Protocol exposing the specific type
    -- | Use this when you need the specific instance
    mkConsensusProtocolExample

    -- * Protocols hiding the specific type
    -- | Use this when you want to handle protocols generically
  , mkSomeConsensusProtocolExample

    -- * Errors
  , ExampleProtocolInstantiationError(..)
  , renderExampleProtocolInstantiationError
  ) where

import           Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as T

import           Cardano.Slotting.Slot (EpochNo)

import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Example as Example
import qualified Ouroboros.Consensus.Example.Node as Example
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()

import           Ouroboros.Consensus.Cardano.Condense ()

import           Ouroboros.Consensus.Shelley.Protocol (StandardCrypto)

import           Cardano.Node.Types

import           Cardano.Tracing.OrphanInstances.Shelley ()

import qualified Cardano.Node.Protocol.Shelley as Shelley

import           Cardano.Node.Protocol.Types

------------------------------------------------------------------------------
-- Example Cardano protocol
--
-- This transitions from the Shelley era, with which it originates,
-- to the Example era.
--

-- The Example version of NodeHardForkProtocolConfiguration from Cardano.Node.Types
-- There is only one transition, so it's a bit simpler
data NodeExampleHardForkProtocolConfiguration =
     NodeExampleHardForkProtocolConfiguration {

       -- | For testing purposes we support specifying that the hard fork
       -- happens at an exact epoch number (ie the first epoch of the new era).
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
       npcTestExampleHardForkAtEpoch :: Maybe EpochNo

       -- | For testing purposes we support specifying that the hard fork
       -- happens at a given major protocol version. For example this can be
       -- used to cause the Shelley hard fork to occur at the transition from
       -- protocol version 0 to version 1 (rather than the default of from 1 to
       -- 2) which can make the test setup simpler.
       --
       -- Obviously if this is used, all the nodes in the test cluster must be
       -- configured the same, or they will disagree.
       --
     , npcTestExampleHardForkAtVersion :: Maybe Word
     }
  deriving (Eq, Show)

-- | Make 'SomeConsensusProtocol' using the Cardano instance.
--
-- The Cardano protocol instance is currently the sequential composition of
-- the Byron and Shelley protocols, and will likely be extended in future
-- with further sequentially composed protocol revisions.
--
-- The use of 'SomeConsensusProtocol' lets us handle multiple protocols in a
-- generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolExample
  :: NodeShelleyProtocolConfiguration
  -> NodeExampleHardForkProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ExampleProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolExample ncs nch files =

    -- Applying the SomeConsensusProtocol here is a check that
    -- the type of mkConsensusProtocolExample fits all the class
    -- constraints we need to run the protocol.
    SomeConsensusProtocol
      <$> mkConsensusProtocolExample ncs nch files


-- | Instantiate 'Consensus.Protocol' for Byron specifically.
--
-- Use this when you need to run the consensus with this specific protocol.
--
mkConsensusProtocolExample
  :: NodeShelleyProtocolConfiguration
  -> NodeExampleHardForkProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT ExampleProtocolInstantiationError IO
             (Consensus.Protocol IO (Example.ExampleBlock StandardCrypto)
                                    Example.ProtocolExample)
mkConsensusProtocolExample 
                           NodeShelleyProtocolConfiguration {
                             npcShelleyGenesisFile,
                             npcShelleyGenesisFileHash
                           }
                           NodeExampleHardForkProtocolConfiguration {
                             npcTestExampleHardForkAtEpoch,
                             npcTestExampleHardForkAtVersion
                           }
                           files = do
    (shelleyGenesis, shelleyGenesisHash) <-
      firstExceptT ExampleProtocolInstantiationErrorShelley $
        Shelley.readGenesis npcShelleyGenesisFile
                            npcShelleyGenesisFileHash

    shelleyLeaderCredentials <-
      firstExceptT ExampleProtocolInstantiationErrorShelley $
        Shelley.readLeaderCredentials files

    let transitionTrigger =
          case npcTestExampleHardForkAtEpoch of

             -- This specifies the major protocol version number update that will
             -- trigger us moving to the Example protocol.
             --
             -- Version 0 is Shelley
             -- Version 1 is Example
             --
             -- But we also provide an override to allow for simpler test setups
             -- such as triggering at the 0 -> 1 transition .
             --
             Nothing -> Consensus.TriggerHardForkAtVersion
                          (maybe 1 fromIntegral npcTestExampleHardForkAtVersion)

             -- Alternatively, for testing we can transition at a specific epoch.
             --
             Just epochNo -> Consensus.TriggerHardForkAtEpoch epochNo

    --TODO: all these protocol versions below are confusing and unnecessary.
    -- It could and should all be automated and these config entries eliminated.
    let protocolExample = Consensus.ProtocolExample
          Consensus.ProtocolParamsShelleyBased {
            shelleyBasedGenesis = shelleyGenesis,
            shelleyBasedInitialNonce =
              Shelley.genesisHashToPraosNonce shelleyGenesisHash,
            shelleyBasedLeaderCredentials = shelleyLeaderCredentials
          }
          Consensus.ProtocolParamsShelley {
            -- This is /not/ the Shelley protocol version. It is the protocol
            -- version that this node will declare that it understands, when it
            -- is in the Shelley era. That is, it is the version of protocol
            -- /after/ Shelley, i.e. Example.
            shelleyProtVer =
              ProtVer 1 0
          }
          Example.ProtocolParamsExample {
            -- This is /not/ the Example protocol version. It is the protocol
            -- version that this node will declare that it understands, when it
            -- is in the Example era, which is the last known protocol of this consensus,
            -- i.e. Example.
            Example.exampleProtVer =
              ProtVer 1 0
          }
          -- ProtocolParamsTransition specifies the parameters needed to transition between two eras
          -- Shelley to Example hard fork parameters
          (Example.ProtocolParamsTransition transitionTrigger)

    pure $! protocolExample

------------------------------------------------------------------------------
-- Errors
--

data ExampleProtocolInstantiationError =
       ExampleProtocolInstantiationErrorShelley
         Shelley.ShelleyProtocolInstantiationError

     | ExampleProtocolInstantiationErrorExample
         Shelley.ShelleyProtocolInstantiationError
  deriving Show

renderExampleProtocolInstantiationError :: ExampleProtocolInstantiationError
                                        -> T.Text
renderExampleProtocolInstantiationError
  (ExampleProtocolInstantiationErrorShelley err) =
    "Shelley: " <> Shelley.renderShelleyProtocolInstantiationError err

renderExampleProtocolInstantiationError
  (ExampleProtocolInstantiationErrorExample err) =
    "Example: " <> Shelley.renderShelleyProtocolInstantiationError err
