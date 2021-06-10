{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Protocol.Example
  ( mkSomeConsensusProtocolExample

    -- * Errors
  , ExampleProtocolInstantiationError(..)
  , renderExampleProtocolInstantiationError
  ) where

import           Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as T

import           Ouroboros.Consensus.Cardano hiding (Protocol)
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Example as Example
import qualified Ouroboros.Consensus.Example.Node as Example
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()

import           Ouroboros.Consensus.Cardano.Condense ()

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
mkSomeConsensusProtocolExample
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
             Nothing -> Example.TriggerHardForkAtVersion
                          (maybe 1 fromIntegral npcTestExampleHardForkAtVersion)

             -- Alternatively, for testing we can transition at a specific epoch.
             --
             Just epochNo -> Example.TriggerHardForkAtEpoch epochNo

    --TODO: all these protocol versions below are confusing and unnecessary.
    -- It could and should all be automated and these config entries eliminated.
    let protocolExample = SomeConsensusProtocol ExampleBlockType $ Example.RunProtocolExample
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
