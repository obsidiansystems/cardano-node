{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- | Let genesis delegates vote on MIRs, the same way they vote on protocol paremter updates.
module Cardano.Api.MirProposal (
    EpochNo,

    -- * Update proposals to change the protocol paramaters
    MirProposal(..),
    makeVoltaireMirProposal,

    -- * Internal conversion functions
    toPrototypeTwoMirProposal,

    -- * Data family instances
    AsType(..)
  ) where

import           Prelude

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Sequence.Strict as Seq
import Data.Coders
import           Cardano.Slotting.Slot (EpochNo)
import           Ouroboros.Consensus.Shelley.Protocol.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Voltaire.Prototype.Class as Voltaire
import qualified Cardano.Ledger.Voltaire.Prototype.Class as Prototype
import qualified Cardano.Api.Prototype.Tmp as Prototype
import qualified Cardano.Ledger.Voltaire.Prototype.One as One
import qualified Cardano.Ledger.Voltaire.Prototype.Two as Two

import           Cardano.Api.Certificate (MIRTarget, toShelleyMirCertificate, fromShelleyMirCertificate)
import           Cardano.Api.Address
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.KeysByron
import           Cardano.Api.KeysShelley
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.TxMetadata
import           Cardano.Api.Value
import Shelley.Spec.Ledger.TxBody (MIRPot, MIRCert)
import Shelley.Spec.Ledger.API (KeyRole(Genesis), KeyHash)

-- ----------------------------------------------------------------------------
-- MIR proposal embedded in a transaction.
--
-- Similar to 'Cardano.Api.ProtocolParameters.UpdateProposal'. The difference
-- being what is voted on: 'Cardano.Api.ProtocolParameters.ProtocolParametersUpdate'
-- versus a MIR transfer.
--

data MirProposal =
     MirProposal
       !(Map (Hash GenesisKey) (MIRPot, MIRTarget))
       !EpochNo
    deriving stock (Eq, Show)
    deriving anyclass SerialiseAsCBOR

instance HasTypeProxy MirProposal where
    data AsType MirProposal = AsMirProposal
    proxyToAsType _ = AsMirProposal

instance HasTextEnvelope MirProposal where
    textEnvelopeType _ = "MirProposalVoltaire"

instance ToCBOR MirProposal where
  toCBOR (MirProposal m s) =
    let m' = fmap (uncurry toShelleyMirCertificate) $
          Map.mapKeysMonotonic (\(GenesisKeyHash kh) -> kh) m
    in encode $ Rec (,) !> To m' !> To s

instance FromCBOR MirProposal where
  fromCBOR =
    let convMap m = fmap fromShelleyMirCertificate $
          Map.mapKeysMonotonic GenesisKeyHash m
        conv (m, s) = MirProposal (convMap m) s
    in conv <$> (decode $ RecD (,) <! From <! From)

makeVoltaireMirProposal :: (MIRPot, MIRTarget)
                        -> [Hash GenesisKey]
                        -> EpochNo
                        -> MirProposal
makeVoltaireMirProposal params genesisKeyHashes =
    MirProposal (Map.fromList [ (kh, params) | kh <- genesisKeyHashes ])

toPrototypeTwoMirProposal :: MirProposal -> Voltaire.Update Prototype.StandardVoltaireTwo
toPrototypeTwoMirProposal (MirProposal map' epochNo) =
  Prototype.emptyUpdate {
    Prototype._update_submissions = Prototype.Submissions $ Seq.fromList proposalList
  }
 where
  proposalList = toPrototypeTwoMirProposals epochNo map'

toPrototypeTwoMirProposals
  :: EpochNo
  -> Map (Hash GenesisKey) (MIRPot, MIRTarget)
  -> [Prototype.Proposal Prototype.StandardVoltaireTwo]
toPrototypeTwoMirProposals epochNo ppupMap =
    map (uncurry toProposal)
  $ Map.toList (toShelleyMirCert ppupMap)
 where
  toProposal keyHash mirCert =
    Prototype.Proposal (One.ProposalHeader keyHash epochNo) (Two.BodyMIR mirCert)

toShelleyMirCert
    :: Map (Hash GenesisKey) (MIRPot, MIRTarget)
    -> Map (KeyHash 'Genesis StandardCrypto) (MIRCert StandardCrypto)
toShelleyMirCert =
    Map.mapKeysMonotonic (\(GenesisKeyHash kh) -> kh)
  . Map.map (uncurry toShelleyMirCertificate)
