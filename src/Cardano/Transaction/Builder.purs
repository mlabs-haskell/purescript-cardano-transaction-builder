module Cardano.Transaction.Builder
  ( TransactionBuilderStep
      ( SpendOutput
      , Pay
      , MintAsset
      , IssueCertificate
      , WithdrawRewards
      )
  , OutputWitness(NativeScriptOutput, PlutusScriptOutput)
  , CredentialWitness(NativeScriptCredential, PlutusScriptCredential)
  , ScriptWitness(ScriptValue, ScriptReference)
  , DatumWitness(DatumValue, DatumReference)
  , RefInputAction(ReferenceInput, SpendInput)
  , TxBuildError
      ( WrongSpendWitnessType
      , IncorrectDatumHash
      , IncorrectScriptHash
      , WrongOutputType
      , WrongStakeCredentialType
      , DatumWitnessNotProvided
      , UnneededDatumWitness
      , UnneededDeregisterWitness
      , UnableToAddMints
      , RedeemerIndexingError
      , RedeemerIndexingInternalError
      , WrongNetworkId
      , NoTransactionNetworkId
      )
  , ExpectedWitnessType(ScriptHashWitness, PubKeyHashWitness)
  , CredentialAction
      ( StakeCert
      , Withdrawal
      , Minting
      )
  , buildTransaction
  , modifyTransaction
  , explainTxBuildError
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Transaction.Edit
  ( DetachedRedeemer
  , RedeemerPurpose(ForCert, ForReward, ForSpend, ForMint)
  , fromEditableTransactionSafe
  , toEditableTransactionSafe
  )
import Cardano.Types
  ( Address
  , AssetName
  , Coin
  , DataHash
  , Mint
  , NativeScript
  , NetworkId
  , PaymentCredential(PaymentCredential)
  , PlutusData
  , PlutusScript
  , Redeemer
  , RewardAddress
  , StakeCredential(StakeCredential)
  , Transaction
  , TransactionBody
  , _certs
  , _inputs
  , _mint
  , _networkId
  , _outputs
  , _referenceInputs
  , _withdrawals
  )
import Cardano.Types.Address (getNetworkId, getPaymentCredential)
import Cardano.Types.Certificate
  ( Certificate
      ( StakeRegistration
      , PoolRegistration
      , PoolRetirement
      , StakeDeregistration
      , GenesisKeyDelegation
      , StakeDelegation
      , MoveInstantaneousRewardsCert
      )
  )
import Cardano.Types.Credential
  ( Credential(PubKeyHashCredential, ScriptHashCredential)
  )
import Cardano.Types.Credential as Credential
import Cardano.Types.DataHash as PlutusData
import Cardano.Types.Int as Int
import Cardano.Types.Mint as Mint
import Cardano.Types.NativeScript as NativeScript
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash, OutputDatum))
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.RedeemerDatum (RedeemerDatum)
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.StakeCredential (StakeCredential)
import Cardano.Types.StakePubKeyHash (StakePubKeyHash)
import Cardano.Types.Transaction (_body, _witnessSet)
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput, _address, _datum)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput, _output)
import Cardano.Types.TransactionWitnessSet (_nativeScripts, _plutusData, _plutusScripts)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, modify_, runStateT)
import Control.Monad.State.Trans (gets)
import Data.Array (nub)
import Data.Bifunctor (lmap)
import Data.ByteArray (byteArrayToHex)
import Data.Either (Either(Left, Right), either, note)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view, (%=), (.~), (^.))
import Data.Lens.Record (prop)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (for_, traverse_)
import Data.Tuple (snd)
import Record (merge)
import Type.Proxy (Proxy(Proxy))

data TransactionBuilderStep
  = SpendOutput TransactionUnspentOutput (Maybe OutputWitness)
  | Pay TransactionOutput
  | MintAsset ScriptHash AssetName Int.Int CredentialWitness
  | IssueCertificate Certificate (Maybe CredentialWitness)
  | WithdrawRewards StakeCredential Coin (Maybe CredentialWitness)

derive instance Generic TransactionBuilderStep _
derive instance Eq TransactionBuilderStep
instance Show TransactionBuilderStep where
  show = genericShow

-- | `OutputWitness` is used to provide the evidence needed to consume an
-- | output. It must correspond to a `TransactionUnspentOutput` address'
-- | payment credential to unlock it.
data OutputWitness
  = NativeScriptOutput (ScriptWitness NativeScript)
  | PlutusScriptOutput (ScriptWitness PlutusScript) RedeemerDatum
      (Maybe DatumWitness)

derive instance Generic OutputWitness _
derive instance Eq OutputWitness
instance Show OutputWitness where
  show = genericShow

-- | `CredentialWitness` is used to provide the evidence needed to perform
-- | operations on behalf of a credential, which include:
-- |
-- | - Minting
-- | - Certificate witnessing
-- | - Rewards withdrawal
-- |
-- | Unlike `OutputWitness`, it does not include a `DatumWitness`, because
-- | minting policies and stake scripts do not have a datum.
data CredentialWitness
  = NativeScriptCredential (ScriptWitness NativeScript)
  | PlutusScriptCredential (ScriptWitness PlutusScript) RedeemerDatum

derive instance Eq CredentialWitness
derive instance Generic CredentialWitness _
instance Show CredentialWitness where
  show = genericShow

-- | Gives the user options for specifying everything needed to spend an UTxO
-- | located at an address with a ScriptHash payment credential.
-- |
-- | - `ScriptValue` contains a script for the witness set.
-- |
-- | - `ScriptReference` contains a CIP-31 reference input where the inline script should be available at, and a flag to either spend the referenced input or just reference it.
data ScriptWitness a
  = ScriptValue a
  | ScriptReference TransactionInput RefInputAction

derive instance Show a => Generic (ScriptWitness a) _
derive instance Eq a => Eq (ScriptWitness a)
instance Show a => Show (ScriptWitness a) where
  show = genericShow

-- | Inputs can be referenced or spent in a transaction (See CIP-31).
-- | Inline datums (CIP-32) and reference scripts (CIP-33) contained within
-- | transaction outputs become visible to the script context of the
-- | transaction, regardless of whether the output is spent or just
-- | referenced. This data type lets the developer to specify, which
-- | action to perform with a referenced input.
data RefInputAction
  = ReferenceInput
  | SpendInput

derive instance Generic RefInputAction _
derive instance Eq RefInputAction
instance Show RefInputAction where
  show = genericShow

-- | Datums in UTxOs can be stored in two forms: inline datums or datum hashes.
-- | When there's a hash, we need to provide a datum corresponding to this hash,
-- | which can be done by either providing the value literally, or using a
-- | reference input where it is stored inline.
data DatumWitness
  = DatumValue PlutusData
  | DatumReference TransactionInput RefInputAction

derive instance Generic DatumWitness _
derive instance Eq DatumWitness
instance Show DatumWitness where
  show = genericShow

data StakeWitness
  = PubKeyHashStakeWitness StakePubKeyHash
  | PlutusScriptStakeWitness (ScriptWitness PlutusScript)
  | NativeScriptStakeWitness (ScriptWitness NativeScript)

derive instance Generic StakeWitness _
instance Show StakeWitness where
  show = genericShow

type Context =
  { transaction :: Transaction
  , redeemers :: Array DetachedRedeemer
  , networkId :: Maybe NetworkId
  }

_transaction
  :: Lens' Context Transaction
_transaction = prop (Proxy :: Proxy "transaction")

_redeemers
  :: Lens' Context (Array DetachedRedeemer)
_redeemers = prop (Proxy :: Proxy "redeemers")

data ExpectedWitnessType = ScriptHashWitness | PubKeyHashWitness

derive instance Generic ExpectedWitnessType _
derive instance Eq ExpectedWitnessType
instance Show ExpectedWitnessType where
  show = genericShow

explainExpectedWitnessType :: ExpectedWitnessType -> String
explainExpectedWitnessType ScriptHashWitness = "ScriptHash"
explainExpectedWitnessType PubKeyHashWitness = "PubKeyHash"

data CredentialAction
  = StakeCert Certificate
  | Withdrawal RewardAddress
  | Minting ScriptHash

derive instance Generic CredentialAction _
derive instance Eq CredentialAction
instance Show CredentialAction where
  show = genericShow

explainCredentialAction :: CredentialAction -> String
explainCredentialAction (StakeCert _) = "This stake certificate"
explainCredentialAction (Withdrawal _) = "This stake rewards withdrawal"
explainCredentialAction (Minting _) = "This mint"

data TxBuildError
  = WrongSpendWitnessType TransactionUnspentOutput
  | IncorrectDatumHash TransactionUnspentOutput PlutusData DataHash
  | IncorrectScriptHash (Either NativeScript PlutusScript) ScriptHash
  | WrongOutputType ExpectedWitnessType TransactionUnspentOutput
  | WrongStakeCredentialType CredentialAction ExpectedWitnessType
      StakeCredential
  | DatumWitnessNotProvided TransactionUnspentOutput
  | UnneededDatumWitness TransactionUnspentOutput DatumWitness
  | UnneededDeregisterWitness StakeCredential CredentialWitness
  | UnableToAddMints Mint Mint
  | RedeemerIndexingError Redeemer
  | RedeemerIndexingInternalError Transaction (Array TransactionBuilderStep)
  | WrongNetworkId Address
  | NoTransactionNetworkId

derive instance Generic TxBuildError _
derive instance Eq TxBuildError
instance Show TxBuildError where
  show = genericShow

explainTxBuildError :: TxBuildError -> String
explainTxBuildError (WrongSpendWitnessType utxo) =
  "`OutputWitness` is incompatible with the given output. The output does not contain a datum: "
    <> show utxo
explainTxBuildError (IncorrectDatumHash utxo datum datumHash) =
  "You provided a `DatumWitness` with a datum that does not match the datum hash present in a transaction output.\n  Datum: "
    <> show datum
    <> " (CBOR: "
    <> byteArrayToHex (unwrap $ encodeCbor datum)
    <> ")\n  Datum hash: "
    <> byteArrayToHex (unwrap $ encodeCbor datumHash)
    <> "\n  UTxO: "
    <> show utxo
explainTxBuildError (IncorrectScriptHash (Left nativeScript) hash) =
  "Provided script hash (" <> show hash <> ") does not match the provided native script (" <> show nativeScript <> ")"
explainTxBuildError (IncorrectScriptHash (Right plutusScript) hash) =
  "Provided script hash (" <> show hash <> ") does not match the provided Plutus script (" <> show plutusScript <> ")"
explainTxBuildError (WrongOutputType ScriptHashWitness utxo) =
  "The UTxO you provided requires no witness, because the payment credential of the address is a `PubKeyHash`. UTxO: " <> show
    utxo
explainTxBuildError (WrongOutputType PubKeyHashWitness utxo) =
  "The UTxO you provided requires a `ScriptHash` witness to unlock, because the payment credential of the address is a `ScriptHash`. UTxO: " <>
    show utxo
explainTxBuildError
  (WrongStakeCredentialType operation expWitnessType stakeCredential) =
  explainCredentialAction operation <> " (" <> show operation <> ") requires a "
    <> explainExpectedWitnessType expWitnessType
    <> " witness: "
    <> show stakeCredential
explainTxBuildError (DatumWitnessNotProvided utxo) =
  "The UTxO you are trying to spend contains a datum hash. A matching `DatumWitness` is required. Use `getDatumByHash`. UTxO: "
    <> show utxo
explainTxBuildError (UnneededDatumWitness utxo witness) =
  "You've provided an optional `DatumWitness`, but the output you are spending already contains an inline datum (not just a datum hash). You should omit the provided datum witness. You provided: "
    <> show witness
    <> " for the UTxO: "
    <> show utxo
explainTxBuildError (UnneededDeregisterWitness stakeCredential witness) =
  "You've provided an optional `CredentialWitness`, but the stake credential you are trying to issue a deregistering certificate for is a PubKeyHash credential. You should omit the provided credential witness for this credential: " <> show stakeCredential <> ". Provided witness: " <> show witness
explainTxBuildError (UnableToAddMints a b) =
  "Numeric overflow: unable to add `Mint`s: " <> show a <> " and " <> show b
explainTxBuildError (RedeemerIndexingError redeemer) =
  "Redeemer indexing error. Problematic redeemer that does not have a valid index: " <> show redeemer
explainTxBuildError (RedeemerIndexingInternalError tx steps) =
  "Internal redeemer indexing error. Please report as bug: " <> bugTrackerUrl <> "\nDebug info: Transaction: " <> show tx <> ", steps: " <> show steps
explainTxBuildError (WrongNetworkId address) =
  "The following `Address` that was specified in one of the UTxOs has a `NetworkId` different from the one `TransactionBody` has: " <> show address
explainTxBuildError NoTransactionNetworkId =
  "You are editing a transaction without a `NetworkId` set. To create a `RewardAddress`, a NetworkId is needed: set it in the `TransactionBody`"

type BuilderM a = StateT Context (Except TxBuildError) a

buildTransaction
  :: Array TransactionBuilderStep
  -> Either TxBuildError Transaction
buildTransaction =
  modifyTransaction Transaction.empty

modifyTransaction
  :: Transaction
  -> Array TransactionBuilderStep
  -> Either TxBuildError Transaction
modifyTransaction tx steps = do
  context <- do
    editableTransaction <- lmap RedeemerIndexingError $ toEditableTransactionSafe tx
    pure $ merge editableTransaction
      { networkId: editableTransaction.transaction ^. _body <<< _networkId }
  let
    eiCtx = map snd
      $ runExcept
      $ flip runStateT context
      $ processConstraints steps
  eiCtx >>= \({ redeemers, transaction }) ->
    note (RedeemerIndexingInternalError tx steps) $
      fromEditableTransactionSafe { redeemers, transaction }

processConstraints :: Array TransactionBuilderStep -> BuilderM Unit
processConstraints = traverse_ processConstraint

processConstraint :: TransactionBuilderStep -> BuilderM Unit
processConstraint = case _ of
  SpendOutput utxo spendWitness -> do
    assertNetworkId $ utxo ^. _output <<< _address
    _transaction <<< _body <<< _inputs
      %= pushUnique (unwrap utxo).input
    useSpendWitness utxo spendWitness
  Pay output -> do
    assertNetworkId $ output ^. _address
    _transaction <<< _body <<< _outputs
      -- intentionally not using pushUnique: we can
      -- create multiple outputs of the same shape
      %= flip append [ output ]
  MintAsset scriptHash assetName amount mintWitness -> do
    useMintAssetWitness scriptHash assetName amount mintWitness
  IssueCertificate cert witness -> do
    useCertificateWitness cert witness
  WithdrawRewards stakeCredential amount witness -> do
    useWithdrawRewardsWitness stakeCredential amount witness

assertNetworkId :: Address -> BuilderM Unit
assertNetworkId addr = do
  mbNetworkId <- gets _.networkId
  let
    addrNetworkId = getNetworkId addr
  case mbNetworkId of
    Nothing -> pure unit
    Just networkId -> do
      unless (networkId == addrNetworkId) do
        throwError (WrongNetworkId addr)

assertOutputType :: ExpectedWitnessType -> TransactionUnspentOutput -> BuilderM Unit
assertOutputType outputType utxo = do
  let
    mbCredential =
      (getPaymentCredential (utxo ^. _output <<< _address) <#> unwrap)
        >>= case outputType of
          ScriptHashWitness -> Credential.asScriptHash >>> void
          PubKeyHashWitness -> Credential.asPubKeyHash >>> void
  unless (isJust mbCredential) do
    throwError $ WrongOutputType outputType utxo

assertStakeCredentialType
  :: CredentialAction -> ExpectedWitnessType -> StakeCredential -> BuilderM Unit
assertStakeCredentialType action expectedType credential = do
  let
    mbCredential =
      case expectedType of
        ScriptHashWitness ->
          void $ Credential.asScriptHash $ unwrap credential
        PubKeyHashWitness ->
          void $ Credential.asPubKeyHash $ unwrap credential
  unless (isJust mbCredential) do
    throwError $ WrongStakeCredentialType action expectedType credential

useMintAssetWitness
  :: ScriptHash -> AssetName -> Int.Int -> CredentialWitness -> BuilderM Unit
useMintAssetWitness scriptHash assetName amount witness = do
  useCredentialWitness
    (Minting scriptHash)
    (wrap $ ScriptHashCredential scriptHash)
    (Just witness)
  mbMint <- gets $ view $ _transaction <<< _body <<< _mint
  let
    thisMint = Mint.singleton scriptHash assetName amount
  newMint <- case mbMint of
    Nothing -> pure thisMint
    Just mint -> Mint.union mint thisMint #
      maybe (throwError $ UnableToAddMints mint thisMint) pure
  modify_ $ _transaction <<< _body <<< _mint .~ Just newMint

assertScriptHashMatchesCredentialWitness :: ScriptHash -> CredentialWitness -> BuilderM Unit
assertScriptHashMatchesCredentialWitness scriptHash witness = do
  let
    mbScript = case witness of
      PlutusScriptCredential (ScriptValue plutusScript) _ -> Just (Right plutusScript)
      NativeScriptCredential (ScriptValue nativeScript) -> Just (Left nativeScript)
      _ -> Nothing
  for_ mbScript \eiScript -> do
    let hash = either NativeScript.hash PlutusScript.hash eiScript
    unless (scriptHash == hash) do
      throwError $ IncorrectScriptHash eiScript scriptHash

useCertificateWitness :: Certificate -> Maybe CredentialWitness -> BuilderM Unit
useCertificateWitness cert mbWitness = do
  _transaction <<< _body <<< _certs %= pushUnique cert
  case cert of
    StakeDeregistration stakeCredential -> do
      case stakeCredential, mbWitness of
        StakeCredential (PubKeyHashCredential _), Just witness -> do
          throwError $ UnneededDeregisterWitness stakeCredential witness
        StakeCredential (PubKeyHashCredential _), Nothing -> pure unit
        StakeCredential (ScriptHashCredential _), Nothing -> do
          throwError $
            WrongStakeCredentialType (StakeCert cert) PubKeyHashWitness stakeCredential
        StakeCredential (ScriptHashCredential scriptHash), Just witness -> do
          assertScriptHashMatchesCredentialWitness scriptHash witness
      useCredentialWitness (StakeCert cert) stakeCredential mbWitness
    StakeDelegation stakeCredential _ -> do
      useCredentialWitness (StakeCert cert) stakeCredential mbWitness
    StakeRegistration _ -> pure unit
    PoolRegistration _ -> pure unit
    PoolRetirement _ -> pure unit
    GenesisKeyDelegation _ -> pure unit
    MoveInstantaneousRewardsCert _ -> pure unit

useCredentialWitness
  :: CredentialAction -> StakeCredential -> Maybe CredentialWitness -> BuilderM Unit
useCredentialWitness credentialAction stakeCredential witness = do
  case witness of
    Nothing -> do
      assertStakeCredentialType credentialAction PubKeyHashWitness
        stakeCredential
    Just (NativeScriptCredential nsWitness) -> do
      assertStakeCredentialType credentialAction ScriptHashWitness
        stakeCredential
      useNativeScriptWitness nsWitness
    Just (PlutusScriptCredential plutusScriptWitness redeemerDatum) -> do
      assertStakeCredentialType credentialAction ScriptHashWitness
        stakeCredential
      usePlutusScriptWitness plutusScriptWitness
      let
        redeemer =
          { purpose: case credentialAction of
              Withdrawal rewardAddress -> ForReward rewardAddress
              StakeCert cert -> ForCert cert
              Minting scriptHash -> ForMint scriptHash
          -- ForSpend is not possible: for that we use OutputWitness
          , datum: redeemerDatum
          }
      _redeemers %= pushUnique redeemer

useWithdrawRewardsWitness
  :: StakeCredential -> Coin -> Maybe CredentialWitness -> BuilderM Unit
useWithdrawRewardsWitness stakeCredential amount witness = do
  networkId <- gets _.networkId >>=
    maybe (throwError NoTransactionNetworkId) pure
  let
    rewardAddress =
      { networkId
      , stakeCredential
      }
  _transaction <<< _body <<< _withdrawals %=
    Map.insert rewardAddress amount
  useCredentialWitness (Withdrawal rewardAddress) stakeCredential witness

-- | Tries to modify the transaction to make it consume a given output.
-- | Uses a `SpendWitness` to try to satisfy spending requirements.
useSpendWitness :: TransactionUnspentOutput -> Maybe OutputWitness -> BuilderM Unit
useSpendWitness utxo = case _ of
  Nothing -> do
    assertOutputType PubKeyHashWitness utxo
  Just (NativeScriptOutput nsWitness) -> do
    assertOutputType ScriptHashWitness utxo
    -- attach the script
    useNativeScriptWitness nsWitness
  Just (PlutusScriptOutput plutusScriptWitness redeemerDatum mbDatumWitness) ->
    do
      assertOutputType ScriptHashWitness utxo
      -- attach the script
      usePlutusScriptWitness plutusScriptWitness
      -- attach the datum
      useDatumWitnessForUtxo utxo mbDatumWitness
      -- attach the redeemer
      let
        uiRedeemer =
          { purpose: ForSpend (unwrap utxo).input
          , datum: redeemerDatum
          }
      _redeemers %= pushUnique uiRedeemer

usePlutusScriptWitness :: ScriptWitness PlutusScript -> BuilderM Unit
usePlutusScriptWitness =
  case _ of
    ScriptValue ps -> do
      _transaction <<< _witnessSet <<< _plutusScripts
        %= pushUnique ps
    ScriptReference input action -> do
      _transaction <<< _body <<< refInputActionToLens action
        %= pushUnique input

useNativeScriptWitness :: ScriptWitness NativeScript -> BuilderM Unit
useNativeScriptWitness =
  case _ of
    ScriptValue ns -> do
      _transaction <<< _witnessSet <<< _nativeScripts
        %= pushUnique ns
    ScriptReference refInput refInputAction -> do
      _transaction <<< _body <<< refInputActionToLens refInputAction
        %= pushUnique refInput

-- | Tries to modify the transaction state to make it consume a given script output.
-- | Uses a `DatumWitness` if the UTxO datum is provided as a hash.
useDatumWitnessForUtxo
  :: TransactionUnspentOutput -> Maybe DatumWitness -> BuilderM Unit
useDatumWitnessForUtxo utxo mbDatumWitness = do
  case utxo ^. _output <<< _datum of
    -- script outputs must have a datum
    Nothing -> throwError $ WrongSpendWitnessType utxo
    -- if the datum is inline, we don't need to attach it as witness
    Just (OutputDatum _providedDatum) -> do
      case mbDatumWitness of
        Just datumWitness ->
          throwError $ UnneededDatumWitness utxo datumWitness
        Nothing -> pure unit
    -- if the datum is provided as hash,
    Just (OutputDatumHash datumHash) ->
      case mbDatumWitness of
        -- if the datum witness was not provided, look the datum up
        Nothing -> do
          throwError $ DatumWitnessNotProvided utxo
        -- if the datum was provided, check it's hash. if it matches the one
        -- specified in the output, use that datum.
        Just (DatumValue providedDatum)
          | datumHash == PlutusData.hashPlutusData providedDatum -> do
              _transaction <<< _witnessSet <<< _plutusData
                %= pushUnique providedDatum
        -- otherwise, fail
        Just (DatumValue providedDatum) -> do
          throwError $ IncorrectDatumHash utxo providedDatum datumHash
        -- If a reference input is provided, we just attach it without
        -- checking (doing that would require looking up the utxo)
        --
        -- We COULD require the user to provide the output to do an additional
        -- check, but we don't, because otherwise the contents of the ref output
        -- do not matter (i.e. they are not needed for balancing).
        -- UTxO lookups are quite expensive, so it's best to not require more
        -- of them than strictly necessary.
        Just (DatumReference datumWitnessRef refInputAction) -> do
          _transaction <<< _body <<< refInputActionToLens refInputAction
            %= pushUnique datumWitnessRef

-- | Depending on `RefInputAction` value, we either want to spend a reference
-- | UTxO, or just reference it.
refInputActionToLens
  :: RefInputAction
  -> Lens' TransactionBody (Array TransactionInput)
refInputActionToLens =
  case _ of
    ReferenceInput -> _referenceInputs
    SpendInput -> _inputs

-- | Ensures uniqueness of an element
pushUnique :: forall a. Ord a => a -> Array a -> Array a
pushUnique x xs = nub $ xs <> [ x ]

bugTrackerUrl :: String
bugTrackerUrl = "https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/issues"
