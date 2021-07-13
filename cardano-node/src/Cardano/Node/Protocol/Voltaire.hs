{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans  #-}

module Cardano.Node.Protocol.Voltaire
  ( mkSomeConsensusProtocolVoltaire

    -- * Errors
  , VoltaireProtocolInstantiationError(..)
  , renderVoltaireProtocolInstantiationError
  ) where

import           Prelude

import           Control.Monad.Trans.Except (ExceptT)
import           Control.Monad.Trans.Except.Extra (firstExceptT)
import qualified Data.Text as T


import           Ouroboros.Consensus.Cardano
import qualified Ouroboros.Consensus.Cardano as Consensus
import qualified Ouroboros.Consensus.Cardano.CanHardFork as Consensus
import qualified Ouroboros.Consensus.Voltaire.Prototype.Node as Voltaire
import           Ouroboros.Consensus.HardFork.Combinator.Condense ()

import           Ouroboros.Consensus.Cardano.Condense ()

import           Cardano.Api.Orphans ()
import           Cardano.Api.Protocol.Types
import           Cardano.Node.Types

import           Cardano.Tracing.OrphanInstances.Voltaire ()

import qualified Cardano.Node.Protocol.Shelley as Shelley

import           Cardano.Node.Protocol.Types

------------------------------------------------------------------------------
-- Real Voltaire protocol
--

-- | Make 'SomeConsensusProtocol' using the Voltaire instance.
--
-- The Voltaire protocol instance is currently the sequential composition of
-- the Shelley VoltairePrototype protocols.
--
-- The use of 'SomeConsensusProtocol' lets us handle multiple protocols in a
-- generic way.
--
-- This also serves a purpose as a sanity check that we have all the necessary
-- type class instances available.
--
mkSomeConsensusProtocolVoltaire
  :: NodeShelleyProtocolConfiguration
  -> Maybe ProtocolFilepaths
  -> ExceptT VoltaireProtocolInstantiationError IO SomeConsensusProtocol
mkSomeConsensusProtocolVoltaire NodeShelleyProtocolConfiguration {
                             npcShelleyGenesisFile,
                             npcShelleyGenesisFileHash
                           }
                           files = do
    (shelleyGenesis, shelleyGenesisHash) <-
      firstExceptT VoltaireProtocolInstantiationErrorShelley $
        Shelley.readGenesis npcShelleyGenesisFile
                            npcShelleyGenesisFileHash

    shelleyLeaderCredentials <-
      firstExceptT VoltaireProtocolInstantiationErrorShelley $
        Shelley.readLeaderCredentials files

    --TODO: all these protocol versions below are confusing and unnecessary.
    -- It could and should all be automated and these config entries eliminated.
    return $!
      SomeConsensusProtocol VoltaireBlockType $ ProtocolInfoArgsPrototype
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
          -- /after/ Shelley, i.e. VoltaireOne.
          shelleyProtVer =
            ProtVer 1 0
        }
        Voltaire.ProtocolParamsVoltairePrototype {
          -- This is /not/ the VoltaireOne protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the VoltaireOne era. That is, it is the version of protocol
          -- /after/ VoltaireOne, i.e. VoltaireTwo.
          Voltaire.exampleProtVer = ProtVer 2 0
        }
        Voltaire.ProtocolParamsVoltairePrototype {
          -- This is /not/ the VoltaireTwo protocol version. It is the protocol
          -- version that this node will declare that it understands, when it
          -- is in the VoltaireTwo era. Since VoltaireTwo is currently the last known
          -- protocol version then this is also the VoltaireTwo protocol version.
          Voltaire.exampleProtVer = ProtVer 2 0
        }
        -- ProtocolParamsTransition specifies the parameters needed to transition between two eras
        -- The comments below also apply for the Shelley -> VoltaireOne and VoltaireOne -> VoltaireTwo hard forks.

        -- Shelley to VoltaireOne hard fork parameters
        Voltaire.ProtocolParamsTransition {
          Voltaire.transitionTrigger = Consensus.TriggerHardForkAtVersion 1
        }

        -- VoltaireOne to VoltaireOne hard fork parameters
        Voltaire.ProtocolParamsTransition {
          Voltaire.transitionTrigger = Consensus.TriggerHardForkAtVersion 2
        }

------------------------------------------------------------------------------
-- Errors
--

data VoltaireProtocolInstantiationError =
    VoltaireProtocolInstantiationErrorShelley
        Shelley.ShelleyProtocolInstantiationError
  deriving Show

renderVoltaireProtocolInstantiationError :: VoltaireProtocolInstantiationError
                                         -> T.Text
renderVoltaireProtocolInstantiationError
  (VoltaireProtocolInstantiationErrorShelley err) =
    Shelley.renderShelleyProtocolInstantiationError err
