{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans  #-}
{-# LANGUAGE DeriveAnyClass #-}

module Cardano.Tracing.OrphanInstances.Voltaire () where

import           Cardano.Prelude

import qualified Data.Aeson as Aeson
import qualified Data.Set as Set

import           Cardano.Api.Orphans ()
import           Cardano.Tracing.OrphanInstances.Shelley ()

import           Cardano.Tracing.OrphanInstances.Common
import           Cardano.Tracing.OrphanInstances.Consensus ()


import           Ouroboros.Consensus.Shelley.Ledger hiding (TxId)

import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Crypto as Core
import qualified Cardano.Ledger.Shelley.Constraints as Shelley
import qualified Cardano.Ledger.Voltaire.Prototype.One as One
import qualified Cardano.Ledger.Voltaire.Prototype.Two as Two
import qualified Cardano.Ledger.Voltaire.Prototype.Class as Voltaire
import qualified Cardano.Ledger.Voltaire.Prototype.Rules.Utxo as Voltaire
import qualified Cardano.Ledger.Voltaire.Prototype.Rules.Two.Deleg as Two
import qualified Cardano.Ledger.Voltaire.Prototype.Rules.Two.NewEpoch as Two
import qualified Cardano.Ledger.Voltaire.Prototype.Rules.Two.Upec as Two
import qualified Cardano.Ledger.Voltaire.Prototype.Rules.Two.Mir as Two

-- TODO: this should be exposed via Cardano.Api
import           Shelley.Spec.Ledger.API hiding (ShelleyBasedEra)

import           Shelley.Spec.Ledger.STS.Bbody
import           Shelley.Spec.Ledger.STS.Epoch
import           Shelley.Spec.Ledger.STS.Mir
import           Shelley.Spec.Ledger.STS.Newpp
import           Shelley.Spec.Ledger.STS.PoolReap
import           Shelley.Spec.Ledger.STS.Snap
import           Shelley.Spec.Ledger.STS.Upec
import qualified Cardano.Ledger.Era

{- HLINT ignore "Use :" -}

instance Aeson.ToJSONKey (Voltaire.ProposalId era) => ToObject (One.PpupPredicateFailure era) where
  toObject _verb (One.NonGenesisUpdatePPUP proposalKeys genesisKeys) =
    mkObject [ "kind" .= String "NonGenesisUpdatePPUP"
             , "keys" .= proposalKeys Set.\\ genesisKeys ]
  toObject _verb (One.PPUpdateWrongEpoch currEpoch intendedEpoch votingPeriod) =
    mkObject [ "kind" .= String "PPUpdateWrongEpoch"
             , "currentEpoch" .= currEpoch
             , "intendedEpoch" .= intendedEpoch
             , "votingPeriod"  .= String (show votingPeriod)
             ]
  toObject _verb (One.PVCannotFollowPPUP badPv) =
    mkObject [ "kind" .= String "PVCannotFollowPPUP"
             , "badProtocolVersion" .= badPv
             ]
  toObject _verb (One.UnsupportedVotesPPUP votes) =
    mkObject [ "kind" .= String "UnsupportedVotesPPUP"
             , "unsupportedVotes" .= votes
             ]
  toObject _verb (One.VaryingTargetEpochPPUP firstEpochNo badEpochNo) =
    mkObject [ "kind" .= String "VaryingTargetEpochPPUP"
             , "firstEpochNo" .= firstEpochNo
             , "badEpochNo" .= badEpochNo
             ]
  toObject _verb (One.MultipleProposalsPPUP keyHash) =
    mkObject [ "kind" .= String "MultipleProposalsPPUP"
             , "keyHash" .= keyHash
             ]

deriving instance ToObject (Two.DelegPredicateFailure era)
deriving instance ToJSON (Two.DelegPredicateFailure era)

instance ( ToJSON (PredicateFailure (Core.EraRule "POOLREAP" era))
         , ToJSON (PredicateFailure (Core.EraRule "SNAP" era))
         , ToJSON (PredicateFailure (Core.EraRule "UPEC" era))
         ) => ToJSON (EpochPredicateFailure era)

instance ToJSON (PoolreapPredicateFailure era)
instance ToJSON (SnapPredicateFailure era)
instance ToJSON (UpecPredicateFailure era)
instance ToJSON (NewppPredicateFailure era)
instance ToJSON (MirPredicateFailure era)

deriving instance
  ( Core.Crypto (Cardano.Ledger.Era.Crypto era)
  , ToJSON (Two.PredicateFailure (Core.EraRule "EPOCH" era))
  )
  => ToObject (Two.NewEpochPredicateFailure era)
deriving instance
  ( Core.Crypto (Cardano.Ledger.Era.Crypto era)
  , ToJSON (Two.PredicateFailure (Core.EraRule "EPOCH" era))
  )
  => ToJSON (Two.NewEpochPredicateFailure era)

deriving instance
  ( Core.Crypto (Cardano.Ledger.Era.Crypto era)
  , ToJSON (Two.PredicateFailure (Core.EraRule "MIR" era))
  )
  => ToJSON (Two.UpecPredicateFailure era)

deriving instance Core.Crypto (Cardano.Ledger.Era.Crypto era)
  => ToJSON (Two.DelegMirPredicateFailure era)


deriving instance Aeson.ToJSONKey (Voltaire.ProposalId era)
  => ToJSON (One.PpupPredicateFailure era)
deriving instance ToJSON One.VotingPeriod

deriving instance
  ( ShelleyBasedEra era
  , ToJSON (Core.Value era)
  , ToJSON (Core.TxOut era)
  , ToObject (PredicateFailure (Core.EraRule "PPUP" era))
  , ToJSON (PredicateFailure (Core.EraRule "PPUP" era))
  )
  => ToObject (Voltaire.UtxoPredicateFailure era)
deriving instance
  ( ShelleyBasedEra era
  , ToJSON (Core.Value era)
  , ToJSON (Core.TxOut era)
  , ToObject (PredicateFailure (Core.EraRule "PPUP" era))
  , ToJSON (PredicateFailure (Core.EraRule "PPUP" era))
  )
  => ToJSON (Voltaire.UtxoPredicateFailure era)

deriving instance Aeson.ToJSONKey (Voltaire.ProposalId era) => ToJSON (Voltaire.Votes era)
deriving instance ToJSON Voltaire.VoteIntention

deriving instance ToJSON (Shelley.PParamsDelta era)
  => Aeson.ToJSON (Two.ProposalId era)
deriving instance ToJSON (Shelley.PParamsDelta era)
  => Aeson.ToJSONKey (Two.ProposalId era)
deriving instance ToJSON (MIRCert crypto)
deriving instance ToJSON (MIRTarget crypto)
deriving instance ToJSON MIRPot

deriving instance Aeson.ToJSONKey (PParams' StrictMaybe era)
