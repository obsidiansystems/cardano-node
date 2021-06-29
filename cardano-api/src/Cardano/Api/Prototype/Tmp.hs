{-# LANGUAGE DataKinds #-}

-- NB: Temporarily located here!
--
-- Once everything is working, move this stuff to a voltaire-specific package.

module Cardano.Api.Prototype.Tmp
( StandardVoltaireOne
, StandardVoltaireTwo
)
where

import Cardano.Ledger.Voltaire.Prototype
import Ouroboros.Consensus.Shelley.Protocol.Crypto

type StandardVoltaireOne
  = VoltairePrototypeEra 'VoltairePrototype_One StandardCrypto

type StandardVoltaireTwo
  = VoltairePrototypeEra 'VoltairePrototype_Two StandardCrypto
