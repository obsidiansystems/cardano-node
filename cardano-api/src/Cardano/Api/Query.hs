{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

-- The Shelley ledger uses promoted data kinds which we have to use, but we do
-- not export any from this API. We also use them unticked as nature intended.
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}


-- | Queries from local clients to the node.
--
module Cardano.Api.Query (

    -- * Queries
    QueryInMode(..),
    QueryInEra(..),
    QueryInShelleyBasedEra(..),
    UTxO(..),

    -- * Internal conversion functions
    toConsensusQuery,
    fromConsensusQueryResult,

    -- * Wrapper types used in queries
    SerialisedDebugLedgerState(..),
    ProtocolState(..),

    DebugLedgerState(..),
  ) where

import           Data.Aeson (ToJSON (..), object, (.=))
import           Data.Bifunctor (bimap)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)
import           Data.SOP.Strict (SListI)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Typeable
import           Control.State.Transition.Extended (State)
import           Prelude

import           Ouroboros.Network.Protocol.LocalStateQuery.Client (Some (..))

import qualified Ouroboros.Consensus.HardFork.Combinator as Consensus
import           Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import qualified Ouroboros.Consensus.HardFork.Combinator.AcrossEras as Consensus
import qualified Ouroboros.Consensus.HardFork.Combinator.Degenerate as Consensus

import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import           Ouroboros.Consensus.Cardano.Block (StandardCrypto)
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus
import qualified Ouroboros.Consensus.Shelley.Update as Consensus
import qualified Ouroboros.Consensus.Voltaire.Prototype.Block as Voltaire
import           Ouroboros.Network.Block (Serialised)

import           Cardano.Binary
import qualified Cardano.Chain.Update.Validation.Interface as Byron.Update
import qualified Cardano.Ledger.Core as Core
import qualified Cardano.Ledger.Era as Ledger

import qualified Cardano.Ledger.Shelley.Constraints as Shelley
import qualified Shelley.Spec.Ledger.API as Shelley
import qualified Shelley.Spec.Ledger.LedgerState as Shelley
import qualified Shelley.Spec.Ledger.PParams as Shelley

import           Cardano.Api.Address
import           Cardano.Api.Block
import           Cardano.Api.Certificate
import           Cardano.Api.Eras
import           Cardano.Api.KeysShelley
import           Cardano.Api.Modes
import           Cardano.Api.NetworkId
import           Cardano.Api.Orphans ()
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.TxBody
import           Cardano.Api.Value
import qualified Cardano.Api.Update as Update


-- ----------------------------------------------------------------------------
-- Queries
--

data QueryInMode mode result where

     QueryCurrentEra :: ConsensusModeIsMultiEra mode
                     -> QueryInMode mode AnyCardanoEra

     QueryInEra      :: EraInMode era mode
                     -> QueryInEra era result
                     -> QueryInMode mode (Either EraMismatch result)

--TODO: add support for these
--     QueryEraStart   :: ConsensusModeIsMultiEra mode
--                     -> EraInMode era mode
--                     -> QueryInMode mode (Maybe EraStart)

--     QueryEraHistory :: QueryInMode mode EraHistory

deriving instance Show (QueryInMode mode result)

data QueryInEra era result where
     QueryByronUpdateState :: QueryInEra ByronEra ByronUpdateState

     QueryInShelleyBasedEra :: ShelleyBasedEra era
                            -> QueryInShelleyBasedEra era result
                            -> QueryInEra era result

     QueryInVoltaireEra :: ShelleyBasedEra era
                        -> QueryInVoltaireEra era result
                        -> QueryInEra era result

deriving instance Show (QueryInEra era result)


data QueryInShelleyBasedEra era result where
     QueryChainPoint
       :: QueryInShelleyBasedEra era ChainPoint

     QueryEpoch
       :: QueryInShelleyBasedEra era EpochNo

     QueryGenesisParameters
       :: QueryInShelleyBasedEra era GenesisParameters

     QueryProtocolParameters
       :: QueryInShelleyBasedEra era ProtocolParameters

     QueryProtocolParametersUpdate
       :: (Consensus.ProposedProtocolUpdates (ShelleyLedgerEra era)) ~ Shelley.ProposedPPUpdates (ShelleyLedgerEra era)
       => QueryInShelleyBasedEra era
            (Map (Hash GenesisKey) ProtocolParametersUpdate)

     QueryStakeDistribution
       :: QueryInShelleyBasedEra era (Map (Hash StakePoolKey) Rational)

     QueryUTxO
       :: Maybe (Set AddressAny)
       -> QueryInShelleyBasedEra era (UTxO era)

     QueryStakeAddresses
       :: Set StakeCredential
       -> NetworkId
       -> QueryInShelleyBasedEra era (Map StakeAddress Lovelace,
                                      Map StakeAddress PoolId)

     -- TODO: Need to update ledger-specs dependency to access RewardProvenance
     -- QueryPoolRanking
     --   :: QueryInShelleyBasedEra era RewardProvenance

     QueryDebugLedgerState
       :: QueryInShelleyBasedEra era (SerialisedDebugLedgerState era)

     QueryProtocolState
       :: QueryInShelleyBasedEra era (ProtocolState era)

deriving instance Show (QueryInShelleyBasedEra era result)


data QueryInVoltaireEra era result where
     VoltaireQueryChainPoint
       :: QueryInVoltaireEra era ChainPoint

     VoltaireQueryEpoch
       :: QueryInVoltaireEra era EpochNo

     VoltaireQueryGenesisParameters
       :: QueryInVoltaireEra era GenesisParameters

     VoltaireQueryProtocolParameters
       :: QueryInVoltaireEra era ProtocolParameters

     VoltaireQueryProtocolParametersUpdate
       :: (Update.VoltaireProtocolUpdate era)
       => QueryInVoltaireEra era
            (Map (Hash GenesisKey) ProtocolParametersUpdate)

     VoltaireQueryStakeDistribution
       :: QueryInVoltaireEra era (Map (Hash StakePoolKey) Rational)

     VoltaireQueryUTxO
       :: Maybe (Set AddressAny)
       -> QueryInVoltaireEra era (UTxO era)

     VoltaireQueryStakeAddresses
       :: Set StakeCredential
       -> NetworkId
       -> QueryInVoltaireEra era (Map StakeAddress Lovelace,
                                      Map StakeAddress PoolId)

     VoltaireQueryDebugLedgerState
       :: QueryInVoltaireEra era (SerialisedDebugLedgerState era)

     VoltaireQueryProtocolState
       :: QueryInVoltaireEra era (ProtocolState era)

deriving instance Show (QueryInVoltaireEra era result)


-- ----------------------------------------------------------------------------
-- Wrapper types used in queries
--

--TODO: provide appropriate instances for these types as needed, e.g. JSON

newtype ByronUpdateState = ByronUpdateState Byron.Update.State
  deriving Show

newtype UTxO era = UTxO (Map TxIn (TxOut era))

instance IsCardanoEra era => ToJSON (UTxO era) where
  toJSON (UTxO m) = toJSON m

newtype SerialisedDebugLedgerState era
  = SerialisedDebugLedgerState (Serialised (Shelley.NewEpochState (ShelleyLedgerEra era)))

data DebugLedgerState era where
  DebugLedgerState :: ShelleyLedgerEra era ~ ledgerera => Shelley.NewEpochState ledgerera -> DebugLedgerState era

instance (Typeable era, Shelley.TransLedgerState FromCBOR (ShelleyLedgerEra era)) => FromCBOR (DebugLedgerState era) where
  fromCBOR = DebugLedgerState <$> (fromCBOR :: Decoder s (Shelley.NewEpochState (ShelleyLedgerEra era)))

-- TODO: Shelley based era class!
instance ( IsShelleyBasedEra era
         , ShelleyLedgerEra era ~ ledgerera
         , Consensus.ShelleyBasedEra ledgerera
         , ToJSON (State (Core.EraRule "PPUP" ledgerera))
         , ToJSON (Core.PParams ledgerera)
         , ToJSON (Shelley.PParamsDelta ledgerera)
         , ToJSON (Core.TxOut ledgerera)) => ToJSON (DebugLedgerState era) where
  toJSON (DebugLedgerState newEpochS) = object [ "lastEpoch" .= Shelley.nesEL newEpochS
                                          , "blocksBefore" .= Shelley.nesBprev newEpochS
                                          , "blocksCurrent" .= Shelley.nesBcur newEpochS
                                          , "stateBefore" .= Shelley.nesEs newEpochS
                                          , "possibleRewardUpdate" .= Shelley.nesRu newEpochS
                                          , "stakeDistrib" .= Shelley.nesPd newEpochS
                                          ]

newtype ProtocolState era
  = ProtocolState (Serialised (Shelley.ChainDepState (Ledger.Crypto (ShelleyLedgerEra era))))

toShelleyAddrSet :: CardanoEra era
                 -> Set AddressAny
                 -> Set (Shelley.Addr Consensus.StandardCrypto)
toShelleyAddrSet era =
    Set.fromList
  . map toShelleyAddr
    -- Ignore any addresses that are not appropriate for the era,
    -- e.g. Shelley addresses in the Byron era, as these would not
    -- appear in the UTxO anyway.
  . mapMaybe (anyAddressInEra era)
  . Set.toList


fromUTxO
  :: ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> Shelley.UTxO ledgerera
  -> UTxO era
fromUTxO eraConversion utxo =
  case eraConversion of
    ShelleyBasedEraShelley ->
      let Shelley.UTxO sUtxo = utxo
      in UTxO . Map.fromList . map (bimap fromShelleyTxIn fromShelleyTxOut) $ Map.toList sUtxo
    ShelleyBasedEraAllegra ->
      let Shelley.UTxO sUtxo = utxo
      in UTxO . Map.fromList . map (bimap fromShelleyTxIn (fromTxOut ShelleyBasedEraAllegra)) $ Map.toList sUtxo
    ShelleyBasedEraMary ->
      let Shelley.UTxO sUtxo = utxo
      in UTxO . Map.fromList . map (bimap fromShelleyTxIn (fromTxOut ShelleyBasedEraMary)) $ Map.toList sUtxo
    ShelleyBasedEraVoltairePrototype ->
      let Shelley.UTxO sUtxo = utxo
      in UTxO . Map.fromList . map (bimap fromShelleyTxIn (fromTxOut ShelleyBasedEraVoltairePrototype)) $ Map.toList sUtxo

fromShelleyPoolDistr :: Shelley.PoolDistr StandardCrypto
                     -> Map (Hash StakePoolKey) Rational
fromShelleyPoolDistr =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    Map.fromList
  . map (bimap StakePoolKeyHash Shelley.individualPoolStake)
  . Map.toList
  . Shelley.unPoolDistr

fromShelleyDelegations :: Map (Shelley.Credential Shelley.Staking StandardCrypto)
                              (Shelley.KeyHash Shelley.StakePool StandardCrypto)
                       -> Map StakeCredential PoolId
fromShelleyDelegations =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    -- In this case it may not be: the Ord instances for Shelley.Credential
    -- do not match the one for StakeCredential
    Map.fromList
  . map (bimap fromShelleyStakeCredential StakePoolKeyHash)
  . Map.toList

fromShelleyRewardAccounts :: Shelley.RewardAccounts Consensus.StandardCrypto
                          -> Map StakeCredential Lovelace
fromShelleyRewardAccounts =
    --TODO: write an appropriate property to show it is safe to use
    -- Map.fromListAsc or to use Map.mapKeysMonotonic
    Map.fromList
  . map (bimap fromShelleyStakeCredential fromShelleyLovelace)
  . Map.toList


-- ----------------------------------------------------------------------------
-- Conversions of queries into the consensus types.
--

toConsensusQuery :: forall mode block result.
                    ConsensusBlockForMode mode ~ block
                 => QueryInMode mode result
                 -> Some (Consensus.Query block)
toConsensusQuery (QueryCurrentEra CardanoModeIsMultiEra) =
    Some (Consensus.QueryHardFork Consensus.GetCurrentEra)

toConsensusQuery (QueryCurrentEra PrototypeModeIsMultiEra) =
    Some (Consensus.QueryHardFork Consensus.GetCurrentEra)

toConsensusQuery (QueryInEra ByronEraInByronMode QueryByronUpdateState) =
    Some (Consensus.DegenQuery Consensus.GetUpdateInterfaceState)

toConsensusQuery (QueryInEra ByronEraInCardanoMode QueryByronUpdateState) =
    Some (Consensus.QueryIfCurrentByron Consensus.GetUpdateInterfaceState)

toConsensusQuery (QueryInEra erainmode (QueryInShelleyBasedEra era q)) =
    case erainmode of
      ByronEraInByronMode     -> case era of {}
      ShelleyEraInShelleyMode -> toConsensusQueryShelleyBased erainmode q
      ByronEraInCardanoMode   -> case era of {}
      ShelleyEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      AllegraEraInCardanoMode -> toConsensusQueryShelleyBased erainmode q
      MaryEraInCardanoMode    -> toConsensusQueryShelleyBased erainmode q
      ShelleyEraInPrototypeMode -> toConsensusQueryShelleyBased erainmode q
      VoltairePrototypeOneEraInPrototypeMode -> toConsensusQueryShelleyBased erainmode q

toConsensusQuery (QueryInEra erainmode (QueryInVoltaireEra era q)) =
    case erainmode of
      ByronEraInByronMode     -> case era of {}
      ShelleyEraInShelleyMode -> toConsensusQueryVoltaire erainmode q
      ByronEraInCardanoMode   -> case era of {}
      ShelleyEraInCardanoMode -> toConsensusQueryVoltaire erainmode q
      AllegraEraInCardanoMode -> toConsensusQueryVoltaire erainmode q
      MaryEraInCardanoMode    -> toConsensusQueryVoltaire erainmode q
      ShelleyEraInPrototypeMode -> toConsensusQueryVoltaire erainmode q
      VoltairePrototypeOneEraInPrototypeMode -> toConsensusQueryVoltaire erainmode q

toConsensusQueryShelleyBased
  :: forall era ledgerera mode block xs result.
     ConsensusBlockForEra era ~ Consensus.ShelleyBlock ledgerera
  => Ledger.Crypto ledgerera ~ Consensus.StandardCrypto
  => ConsensusBlockForMode mode ~ block
  => block ~ Consensus.HardForkBlock xs
  => EraInMode era mode
  -> QueryInShelleyBasedEra era result
  -> Some (Consensus.Query block)
toConsensusQueryShelleyBased erainmode QueryChainPoint =
    Some (consensusQueryInEraInMode erainmode Consensus.GetLedgerTip)

toConsensusQueryShelleyBased erainmode QueryEpoch =
    Some (consensusQueryInEraInMode erainmode Consensus.GetEpochNo)

toConsensusQueryShelleyBased erainmode QueryGenesisParameters =
    Some (consensusQueryInEraInMode erainmode Consensus.GetGenesisConfig)

toConsensusQueryShelleyBased erainmode QueryProtocolParameters =
    Some (consensusQueryInEraInMode erainmode Consensus.GetCurrentPParams)

toConsensusQueryShelleyBased erainmode QueryProtocolParametersUpdate =
    Some (consensusQueryInEraInMode erainmode Consensus.GetProposedPParamsUpdates)

toConsensusQueryShelleyBased erainmode QueryStakeDistribution =
    Some (consensusQueryInEraInMode erainmode Consensus.GetStakeDistribution)

toConsensusQueryShelleyBased erainmode (QueryUTxO Nothing) =
    Some (consensusQueryInEraInMode erainmode Consensus.GetUTxO)

toConsensusQueryShelleyBased erainmode (QueryUTxO (Just addrs)) =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetFilteredUTxO addrs'))
  where
    addrs' :: Set (Shelley.Addr Consensus.StandardCrypto)
    addrs' = toShelleyAddrSet (eraInModeToEra erainmode) addrs

toConsensusQueryShelleyBased erainmode (QueryStakeAddresses creds _nId) =
    Some (consensusQueryInEraInMode erainmode
            (Consensus.GetFilteredDelegationsAndRewardAccounts creds'))
  where
    creds' :: Set (Shelley.Credential Shelley.Staking StandardCrypto)
    creds' = Set.map toShelleyStakeCredential creds

toConsensusQueryShelleyBased erainmode QueryDebugLedgerState =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugNewEpochState))

toConsensusQueryShelleyBased erainmode QueryProtocolState =
    Some (consensusQueryInEraInMode erainmode (Consensus.GetCBOR Consensus.DebugChainDepState))

toConsensusQueryVoltaire
  :: forall era ledgerera mode block xs result.
     ConsensusBlockForEra era ~ Consensus.ShelleyBlock ledgerera
  => Ledger.Crypto ledgerera ~ Consensus.StandardCrypto
  => ConsensusBlockForMode mode ~ block
  => block ~ Consensus.HardForkBlock xs
  => EraInMode era mode
  -> QueryInVoltaireEra era result
  -> Some (Consensus.Query block)
toConsensusQueryVoltaire erainmode query =
  case query of
    VoltaireQueryChainPoint ->
      toConsensusQueryShelleyBased erainmode QueryChainPoint
    VoltaireQueryEpoch ->
      toConsensusQueryShelleyBased erainmode QueryEpoch
    VoltaireQueryGenesisParameters ->
      toConsensusQueryShelleyBased erainmode QueryGenesisParameters
    VoltaireQueryProtocolParameters ->
      toConsensusQueryShelleyBased erainmode QueryProtocolParameters
    VoltaireQueryProtocolParametersUpdate ->
      Some (consensusQueryInEraInMode erainmode Consensus.GetProposedPParamsUpdates)
    VoltaireQueryStakeDistribution ->
      toConsensusQueryShelleyBased erainmode QueryStakeDistribution
    VoltaireQueryUTxO arg ->
      toConsensusQueryShelleyBased erainmode (QueryUTxO arg)
    VoltaireQueryStakeAddresses arg1 arg2 ->
      toConsensusQueryShelleyBased erainmode (QueryStakeAddresses arg1 arg2)
    VoltaireQueryDebugLedgerState ->
      toConsensusQueryShelleyBased erainmode QueryDebugLedgerState
    VoltaireQueryProtocolState ->
      toConsensusQueryShelleyBased erainmode QueryProtocolState

consensusQueryInEraInMode
  :: forall era mode erablock modeblock result result' xs.
     ConsensusBlockForEra era   ~ erablock
  => ConsensusBlockForMode mode ~ modeblock
  => modeblock ~ Consensus.HardForkBlock xs
  => Consensus.HardForkQueryResult xs result ~ result'
  => EraInMode era mode
  -> Consensus.Query erablock  result
  -> Consensus.Query modeblock result'
consensusQueryInEraInMode ByronEraInByronMode     = Consensus.DegenQuery
consensusQueryInEraInMode ShelleyEraInShelleyMode = Consensus.DegenQuery
consensusQueryInEraInMode ByronEraInCardanoMode   = Consensus.QueryIfCurrentByron
consensusQueryInEraInMode ShelleyEraInCardanoMode = Consensus.QueryIfCurrentShelley
consensusQueryInEraInMode AllegraEraInCardanoMode = Consensus.QueryIfCurrentAllegra
consensusQueryInEraInMode MaryEraInCardanoMode    = Consensus.QueryIfCurrentMary
consensusQueryInEraInMode ShelleyEraInPrototypeMode = Voltaire.QueryIfCurrentShelley
consensusQueryInEraInMode VoltairePrototypeOneEraInPrototypeMode = Voltaire.QueryIfCurrentVoltairePrototypeOne


-- ----------------------------------------------------------------------------
-- Conversions of query results from the consensus types.
--

fromConsensusQueryResult :: forall mode block result result'.
                            ConsensusBlockForMode mode ~ block
                         => QueryInMode mode result
                         -> Consensus.Query block result'
                         -> result'
                         -> result
fromConsensusQueryResult (QueryCurrentEra CardanoModeIsMultiEra) q' r' =
    case q' of
      Consensus.QueryHardFork Consensus.GetCurrentEra ->
        anyEraInModeToAnyEra (fromConsensusEraIndex CardanoMode r')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryCurrentEra PrototypeModeIsMultiEra) q' r' =
    case q' of
      Consensus.QueryHardFork Consensus.GetCurrentEra ->
        anyEraInModeToAnyEra (fromConsensusEraIndex PrototypeMode r')
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInByronMode
                                     QueryByronUpdateState) q' r' =
    case (q', r') of
      (Consensus.DegenQuery Consensus.GetUpdateInterfaceState,
       Consensus.DegenQueryResult r'') ->
        Right (ByronUpdateState r'')

fromConsensusQueryResult (QueryInEra ByronEraInCardanoMode
                                     QueryByronUpdateState) q' r' =
    case q' of
      Consensus.QueryIfCurrentByron Consensus.GetUpdateInterfaceState ->
        bimap fromConsensusEraMismatch ByronUpdateState r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInByronMode
                                     (QueryInShelleyBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra ShelleyEraInShelleyMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case (q', r') of
      (Consensus.DegenQuery q'', Consensus.DegenQueryResult r'') ->
        Right (fromConsensusQueryResultShelleyBased ShelleyBasedEraShelley q q'' r'')

fromConsensusQueryResult (QueryInEra ByronEraInCardanoMode
                                     (QueryInShelleyBasedEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra ShelleyEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.QueryIfCurrentShelley q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultShelleyBased ShelleyBasedEraShelley q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra AllegraEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.QueryIfCurrentAllegra q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultShelleyBased ShelleyBasedEraAllegra q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra MaryEraInCardanoMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Consensus.QueryIfCurrentMary q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultShelleyBased ShelleyBasedEraMary q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ShelleyEraInPrototypeMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Voltaire.QueryIfCurrentShelley q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultShelleyBased ShelleyBasedEraShelley q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra VoltairePrototypeOneEraInPrototypeMode
                                     (QueryInShelleyBasedEra _era q)) q' r' =
    case q' of
      Voltaire.QueryIfCurrentVoltairePrototypeOne q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultShelleyBased ShelleyBasedEraVoltairePrototype  q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ShelleyEraInPrototypeMode
                                     (QueryInVoltaireEra _era q)) q' r' =
    case q' of
      Voltaire.QueryIfCurrentShelley q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultVoltaire ShelleyBasedEraShelley q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra VoltairePrototypeOneEraInPrototypeMode
                                     (QueryInVoltaireEra _era q)) q' r' =
    case q' of
      Voltaire.QueryIfCurrentVoltairePrototypeOne q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultVoltaire ShelleyBasedEraVoltairePrototype q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra ByronEraInByronMode
                                     (QueryInVoltaireEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra ByronEraInCardanoMode
                                     (QueryInVoltaireEra era _)) _ _ =
    case era of {}

fromConsensusQueryResult (QueryInEra ShelleyEraInShelleyMode
                                     (QueryInVoltaireEra _era q)) q' r' =
    case (q', r') of
      (Consensus.DegenQuery q'', Consensus.DegenQueryResult r'') ->
        Right (fromConsensusQueryResultVoltaire ShelleyBasedEraShelley q q'' r'')

fromConsensusQueryResult (QueryInEra ShelleyEraInCardanoMode
                                     (QueryInVoltaireEra _era q)) q' r' =
    case q' of
      Consensus.QueryIfCurrentShelley q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultVoltaire ShelleyBasedEraShelley q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra AllegraEraInCardanoMode
                                     (QueryInVoltaireEra _era q)) q' r' =
    case q' of
      Consensus.QueryIfCurrentAllegra q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultVoltaire ShelleyBasedEraAllegra q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResult (QueryInEra MaryEraInCardanoMode
                                     (QueryInVoltaireEra _era q)) q' r' =
    case q' of
      Consensus.QueryIfCurrentMary q'' ->
        bimap fromConsensusEraMismatch
              (fromConsensusQueryResultVoltaire ShelleyBasedEraMary q q'')
              r'
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased
  :: forall era ledgerera result result'.
     ShelleyLedgerEra era ~ ledgerera
  => Shelley.PParams ledgerera ~ Core.PParams ledgerera
  => Shelley.PParamsDelta ledgerera ~ Shelley.PParamsUpdate ledgerera
  => Consensus.ShelleyBasedEra ledgerera
  => Ledger.Crypto ledgerera ~ Consensus.StandardCrypto
  => ShelleyBasedEra era
  -> QueryInShelleyBasedEra era result
  -> Consensus.Query (Consensus.ShelleyBlock ledgerera) result'
  -> result'
  -> result
fromConsensusQueryResultShelleyBased _ QueryChainPoint q' point =
    case q' of
      Consensus.GetLedgerTip -> fromConsensusPoint point
      _                      -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryEpoch q' epoch =
    case q' of
      Consensus.GetEpochNo -> epoch
      _                    -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryGenesisParameters q' r' =
    case q' of
      Consensus.GetGenesisConfig -> fromShelleyGenesis
                                      (Consensus.getCompactGenesis r')
      _                          -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryProtocolParameters q' r' =
    case q' of
      Consensus.GetCurrentPParams -> fromShelleyPParams r'
      _                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryProtocolParametersUpdate q' r' =
    case q' of
      Consensus.GetProposedPParamsUpdates -> fromShelleyProposedPPUpdates @ledgerera r'
      _                                   -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryStakeDistribution q' r' =
    case q' of
      Consensus.GetStakeDistribution -> fromShelleyPoolDistr r'
      _                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased shelleyBasedEra' (QueryUTxO Nothing) q' utxo' =
    case q' of
      Consensus.GetUTxO -> fromUTxO shelleyBasedEra' utxo'
      _                 -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased shelleyBasedEra' (QueryUTxO Just{}) q' utxo' =
    case q' of
      Consensus.GetFilteredUTxO{} -> fromUTxO shelleyBasedEra' utxo'
      _                           -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ (QueryStakeAddresses _ nId) q' r' =
    case q' of
      Consensus.GetFilteredDelegationsAndRewardAccounts{}
        -> let (delegs, rwaccs) = r'
           in ( Map.mapKeys (makeStakeAddress nId) $ fromShelleyRewardAccounts rwaccs
              , Map.mapKeys (makeStakeAddress nId) $ fromShelleyDelegations delegs
              )
      _ -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryDebugLedgerState{} q' r' =
    case q' of
      Consensus.GetCBOR Consensus.DebugNewEpochState -> SerialisedDebugLedgerState r'
      _                                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultShelleyBased _ QueryProtocolState q' r' =
    case q' of
      Consensus.GetCBOR Consensus.DebugChainDepState -> ProtocolState r'
      _                                              -> fromConsensusQueryResultMismatch

fromConsensusQueryResultVoltaire
  :: forall era ledgerera result result'.
     ShelleyLedgerEra era ~ ledgerera
  => Shelley.PParams ledgerera ~ Core.PParams ledgerera
  => Shelley.PParamsDelta ledgerera ~ Shelley.PParamsUpdate ledgerera
  => Consensus.ShelleyBasedEra ledgerera
  => Ledger.Crypto ledgerera ~ Consensus.StandardCrypto
  => ShelleyBasedEra era
  -> QueryInVoltaireEra era result
  -> Consensus.Query (Consensus.ShelleyBlock ledgerera) result'
  -> result'
  -> result
fromConsensusQueryResultVoltaire se VoltaireQueryChainPoint =
    fromConsensusQueryResultShelleyBased se QueryChainPoint

fromConsensusQueryResultVoltaire se VoltaireQueryEpoch =
    fromConsensusQueryResultShelleyBased se QueryEpoch

fromConsensusQueryResultVoltaire se VoltaireQueryGenesisParameters =
    fromConsensusQueryResultShelleyBased se QueryGenesisParameters

fromConsensusQueryResultVoltaire se VoltaireQueryProtocolParameters =
    fromConsensusQueryResultShelleyBased se QueryProtocolParameters

fromConsensusQueryResultVoltaire _ VoltaireQueryProtocolParametersUpdate =
    \q' r' ->
    case q' of
      Consensus.GetProposedPParamsUpdates -> Update.toProtocolParametersUpdate (Proxy :: Proxy era) r'
      _                                   -> fromConsensusQueryResultMismatch

fromConsensusQueryResultVoltaire se VoltaireQueryStakeDistribution =
    fromConsensusQueryResultShelleyBased se QueryStakeDistribution

fromConsensusQueryResultVoltaire se (VoltaireQueryUTxO arg) =
    fromConsensusQueryResultShelleyBased se (QueryUTxO arg)

fromConsensusQueryResultVoltaire se (VoltaireQueryStakeAddresses arg1 nId) =
    fromConsensusQueryResultShelleyBased se (QueryStakeAddresses arg1 nId)

fromConsensusQueryResultVoltaire se VoltaireQueryDebugLedgerState =
    fromConsensusQueryResultShelleyBased se QueryDebugLedgerState

fromConsensusQueryResultVoltaire se VoltaireQueryProtocolState =
    fromConsensusQueryResultShelleyBased se QueryProtocolState

-- | This should /only/ happen if we messed up the mapping in 'toConsensusQuery'
-- and 'fromConsensusQueryResult' so they are inconsistent with each other.
--
-- If we do encounter this error it means that 'toConsensusQuery' maps a
-- API query constructor to a certain consensus query constructor but that
-- 'fromConsensusQueryResult' apparently expects a different pairing.
--
-- For example, imagine if 'toConsensusQuery would (incorrectly) map
-- 'QueryChainPoint' to 'Consensus.GetEpochNo' but 'fromConsensusQueryResult'
-- (correctly) expected to find 'Consensus.GetLedgerTip'. This mismatch would
-- trigger this error.
--
-- Such mismatches should be preventable with an appropriate property test.
--
fromConsensusQueryResultMismatch :: a
fromConsensusQueryResultMismatch =
    error "fromConsensusQueryResult: internal query mismatch"


fromConsensusEraMismatch :: SListI xs
                         => Consensus.MismatchEraInfo xs -> EraMismatch
fromConsensusEraMismatch = Consensus.mkEraMismatch
