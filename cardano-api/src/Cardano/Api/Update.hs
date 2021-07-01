{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Cardano.Api.Update
( VoltaireProtocolUpdate(..)
)
where

import Cardano.Prelude

import Cardano.Api.ProtocolParameters (ProtocolParametersUpdate, fromShelleyProposedPPUpdates)
import Ouroboros.Consensus.Shelley.Update (HasProtocolUpdates, ProposedProtocolUpdates)
import Ouroboros.Consensus.Voltaire.Prototype.Eras ()
import Cardano.Api.Hash (Hash)
import Cardano.Api.KeysShelley (GenesisKey)
import Cardano.Api.Eras (VoltairePrototypeEra, ShelleyLedgerEra)


class (HasProtocolUpdates (ShelleyLedgerEra era)) => VoltaireProtocolUpdate era where
  toProtocolParametersUpdate
    :: Proxy era
    -> ProposedProtocolUpdates (ShelleyLedgerEra era)
    -> Map (Hash GenesisKey) ProtocolParametersUpdate

instance VoltaireProtocolUpdate VoltairePrototypeEra where
  toProtocolParametersUpdate _ = fromShelleyProposedPPUpdates
