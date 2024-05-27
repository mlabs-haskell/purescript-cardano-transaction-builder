module Cardano.Transaction.Builder where

import Prelude

import Aeson (class EncodeAeson, encodeAeson)
import Cardano.AsCbor (encodeCbor)
import Cardano.Types (DataHash, NetworkId, PlutusData, PlutusScript, StakeCredential, Transaction, TransactionInput, TransactionUnspentOutput)
import Cardano.Types (Slot)
import Cardano.Types.Address (getPaymentCredential)
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.AssetName (AssetName)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Certificate (Certificate(..))
import Cardano.Types.Certificate (Certificate)
import Cardano.Types.Coin (Coin)
import Cardano.Types.Coin (Coin)
import Cardano.Types.CostModel (CostModel)
import Cardano.Types.Credential (Credential(..))
import Cardano.Types.Credential as Credential
import Cardano.Types.DataHash as PlutusData
import Cardano.Types.Epoch (Epoch)
import Cardano.Types.ExUnits as ExUnits
import Cardano.Types.Int as Int
import Cardano.Types.Int as Int
import Cardano.Types.Language (Language)
import Cardano.Types.Mint (Mint)
import Cardano.Types.Mint as Mint
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.NativeScript (NativeScript)
import Cardano.Types.OutputDatum (OutputDatum(OutputDatumHash, OutputDatum))
import Cardano.Types.PaymentPubKeyHash (PaymentPubKeyHash)
import Cardano.Types.PlutusData (PlutusData)
import Cardano.Types.PlutusScript (PlutusScript)
import Cardano.Types.PoolParams (PoolParams)
import Cardano.Types.PoolPubKeyHash (PoolPubKeyHash)
import Cardano.Types.Redeemer (Redeemer(..))
import Cardano.Types.RedeemerDatum (RedeemerDatum)
import Cardano.Types.RedeemerTag (RedeemerTag(..))
import Cardano.Types.RewardAddress (RewardAddress)
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.StakeCredential (StakeCredential)
import Cardano.Types.StakePubKeyHash (StakePubKeyHash)
import Cardano.Types.Transaction (_body, _isValid, _witnessSet)
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionBody (TransactionBody, _certs, _inputs, _mint, _outputs, _referenceInputs, _requiredSigners, _ttl, _validityStartInterval, _withdrawals)
import Cardano.Types.TransactionInput (TransactionInput)
import Cardano.Types.TransactionOutput (TransactionOutput, _address, _datum)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput, _output)
import Cardano.Types.TransactionWitnessSet (_nativeScripts, _plutusData, _plutusScripts)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (Except, runExcept)
import Control.Monad.State (StateT, modify_, runStateT)
import Control.Monad.State.Trans (gets)
import Data.Array (nub)
import Data.ByteArray (byteArrayToHex)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens', view, (%=), (.=), (.~), (<>=), (^.))
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(Just, Nothing), isJust, maybe)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse_)
import Data.Tuple (snd)
import Type.Proxy (Proxy(..))

type TransactionBuildPlan = Array TransactionBuilderStep

data TransactionBuilderStep
  = SpendOutput TransactionUnspentOutput (Maybe OutputWitness)
  | Pay TransactionOutput
  | MintAsset ScriptHash AssetName Int.Int CredentialWitness
  | RegisterStake StakeCredential
  | IssueCertificate Certificate (Maybe CredentialWitness)
  | WithdrawStake StakeCredential Coin (Maybe CredentialWitness)
  | RequireSignature PaymentPubKeyHash
  | RegisterPool PoolParams
  | RetirePool PoolPubKeyHash Epoch
  | IncludeDatum PlutusData
  | SetTTL (Maybe Slot)
  | SetValidityStartInterval (Maybe Slot)
  | SetIsValid Boolean

derive instance Generic TransactionBuilderStep _
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

-- | Contains a value redeemer corresponds to, different for each possible
-- | `RedeemerTag`.
-- | Allows to uniquely compute redeemer index, given a `RedeemersContext` that
-- | is valid for the transaction.
data RedeemerPurpose
  = ForSpend TransactionInput
  | ForMint ScriptHash
  | ForReward RewardAddress
  | ForCert Certificate

derive instance Generic RedeemerPurpose _
derive instance Eq RedeemerPurpose
derive instance Ord RedeemerPurpose

instance EncodeAeson RedeemerPurpose where
  encodeAeson = case _ of
    ForSpend txo -> encodeAeson { tag: "ForSpend", value: encodeAeson txo }
    ForMint mps -> encodeAeson { tag: "ForMint", value: encodeAeson mps }
    ForReward addr -> encodeAeson { tag: "ForReward", value: encodeAeson addr }
    ForCert cert -> encodeAeson { tag: "ForCert", value: encodeAeson cert }

instance Show RedeemerPurpose where
  show = genericShow

type Context =
  { transaction :: Transaction
  , costModels :: Map Language CostModel
  , redeemers :: Array UnindexedRedeemer
  , datums :: Array PlutusData
  , networkId :: NetworkId
  }

_transaction
  :: Lens' Context Transaction
_transaction = prop (Proxy :: Proxy "transaction")

_redeemers
  :: Lens' Context (Array UnindexedRedeemer)
_redeemers = prop (Proxy :: Proxy "redeemers")

_datums
  :: Lens' Context (Array PlutusData)
_datums = prop (Proxy :: Proxy "datums")

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
  | DatumHashLookupError DataHash
  | IncorrectDatumHash TransactionUnspentOutput PlutusData DataHash
  | WrongOutputType ExpectedWitnessType TransactionUnspentOutput
  | WrongStakeCredentialType CredentialAction ExpectedWitnessType
      StakeCredential
  | DatumWitnessNotProvided TransactionUnspentOutput
  | UnneededDatumWitness TransactionUnspentOutput DatumWitness
  | UnableToAddMints Mint Mint

derive instance Generic TxBuildError _
derive instance Eq TxBuildError
instance Show TxBuildError where
  show = genericShow

explainTxBuildError :: TxBuildError -> String
explainTxBuildError (WrongSpendWitnessType utxo) =
  "`OutputWitness` is incompatible with the given output. The output does not contain a datum: "
    <> show utxo
explainTxBuildError (DatumHashLookupError dataHash) =
  "The UTxO you are trying to spend contains a datum hash. You didn't provide a `DatumWitness` value corresponding to this hash, so CTL tried to look it up, using a database of datums observed on-chain. This lookup failed. Datum hash: "
    <> show dataHash
explainTxBuildError (IncorrectDatumHash utxo datum datumHash) =
  "You provided a `DatumWitness` with a datum that does not match the datum hash present in a transaction output.\n  Datum: "
    <> show datum
    <> " (CBOR: "
    <> byteArrayToHex (unwrap $ encodeCbor datum)
    <> ")\n  Datum hash: "
    <> byteArrayToHex (unwrap $ encodeCbor datumHash)
    <> "\n  UTxO: "
    <> show utxo
explainTxBuildError (WrongOutputType ScriptHashWitness utxo) =
  "The UTxO you provided requires a Script witness to unlock. UTxO: " <> show
    utxo
explainTxBuildError (WrongOutputType PubKeyHashWitness utxo) =
  "The UTxO you provided requires a PubKeyHash witness to unlock. UTxO: " <>
    show utxo
explainTxBuildError
  (WrongStakeCredentialType operation expWitnessType stakeCredential) =
  explainCredentialAction operation <> " requires a "
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
explainTxBuildError _ = "TODO"

type M a = StateT Context (Except TxBuildError) a

-- | A newtype for the unbalanced transaction after creating one with datums
-- | and redeemers not attached.
type UnbalancedTx =
  { transaction :: Transaction -- the unbalanced tx created
  , datums ::
      Array PlutusData -- the array of ordered datums that require attaching
  , redeemers :: Array UnindexedRedeemer
  }

buildTransaction
  :: NetworkId
  -> Map Language CostModel
  -> Array TransactionBuilderStep
  -> Either TxBuildError UnbalancedTx
buildTransaction networkId costModels steps =
  let
    context =
      { transaction: Transaction.empty
      , costModels
      , redeemers: []
      , datums: []
      , networkId
      }
    extract { transaction, redeemers, datums } =
      { transaction, redeemers, datums }
  in
    map (snd >>> extract)
      $ runExcept
      $ flip runStateT context
      $
        processConstraints steps

processConstraints :: Array TransactionBuilderStep -> M Unit
processConstraints = traverse_ processConstraint

processConstraint :: TransactionBuilderStep -> M Unit
processConstraint = case _ of
  -- TODO: check network ID
  SpendOutput utxo spendWitness -> do
    _transaction <<< _body <<< _inputs
      %= pushUnique (unwrap utxo).input
    useSpendWitness utxo spendWitness
  Pay utxo -> do
    _transaction <<< _body <<< _outputs
      -- intentionally not using pushUnique: we can
      -- create multiple outputs of the same shape
      %= flip append [ utxo ]
  MintAsset scriptHash assetName amount mintWitness -> do
    useMintAssetWitness scriptHash assetName amount mintWitness
  RegisterStake stakeCredential -> do
    _transaction <<< _body <<< _certs %= pushUnique
      (StakeRegistration stakeCredential)
  IssueCertificate cert witness -> do
    useCertificateWitness cert witness
  WithdrawStake stakeCredential amount witness -> do
    useWithdrawRewardsWitness stakeCredential amount witness
  RequireSignature ppkh -> do
    _transaction <<< _body <<< _requiredSigners <>=
      [ wrap $ unwrap $ unwrap ppkh ]
  RegisterPool poolParams -> do
    _transaction <<< _body <<< _certs %= pushUnique
      (PoolRegistration poolParams)
  RetirePool poolKeyHash epoch -> do
    _transaction <<< _body <<< _certs %= pushUnique
      (PoolRetirement { poolKeyHash, epoch })
  IncludeDatum datum -> do
    _datums %= pushUnique datum
    _transaction <<< _witnessSet <<< _plutusData
      %= pushUnique datum
  SetTTL slot -> do
    _transaction <<< _body <<< _ttl .= slot
  SetValidityStartInterval slot -> do
    _transaction <<< _body <<< _validityStartInterval .= slot
  SetIsValid isValid -> do
    _transaction <<< _isValid .= isValid

assertOutputType :: ExpectedWitnessType -> TransactionUnspentOutput -> M Unit
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
  :: CredentialAction -> ExpectedWitnessType -> StakeCredential -> M Unit
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
  :: ScriptHash -> AssetName -> Int.Int -> CredentialWitness -> M Unit
useMintAssetWitness scriptHash assetName amount witness = do
  useCredentialWitness (Minting scriptHash)
    (wrap $ ScriptHashCredential scriptHash) $ Just witness
  mbMint <- gets $ view $ _transaction <<< _body <<< _mint
  let
    thisMint = Mint.singleton scriptHash assetName amount
  newMint <- case mbMint of
    Nothing -> pure thisMint
    Just mint -> Mint.union mint thisMint #
      maybe (throwError $ UnableToAddMints mint thisMint) pure
  modify_ $ _transaction <<< _body <<< _mint .~ Just newMint

useCertificateWitness :: Certificate -> Maybe CredentialWitness -> M Unit
useCertificateWitness cert witness = do
  _transaction <<< _body <<< _certs %= pushUnique cert
  case cert of
    StakeDeregistration stakeCredential -> do
      useCredentialWitness (StakeCert cert) stakeCredential witness
    StakeDelegation stakeCredential _ -> do
      useCredentialWitness (StakeCert cert) stakeCredential witness
    StakeRegistration _ -> pure unit
    PoolRegistration _ -> pure unit
    PoolRetirement _ -> pure unit
    GenesisKeyDelegation _ -> pure unit
    MoveInstantaneousRewardsCert _ -> pure unit

useCredentialWitness
  :: CredentialAction -> StakeCredential -> Maybe CredentialWitness -> M Unit
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
        redeemer = UnindexedRedeemer
          { purpose: case credentialAction of
              Withdrawal rewardAddress -> ForReward rewardAddress
              StakeCert cert -> ForCert cert
              Minting scriptHash -> ForMint scriptHash
          , datum: unwrap redeemerDatum
          }
      _redeemers %= pushUnique redeemer

useWithdrawRewardsWitness
  :: StakeCredential -> Coin -> Maybe CredentialWitness -> M Unit
useWithdrawRewardsWitness stakeCredential amount witness = do
  networkId <- gets _.networkId
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
useSpendWitness :: TransactionUnspentOutput -> Maybe OutputWitness -> M Unit
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
        uiRedeemer = UnindexedRedeemer
          { purpose: ForSpend (unwrap utxo).input
          , datum: unwrap redeemerDatum
          }
      _redeemers %= pushUnique uiRedeemer

usePlutusScriptWitness :: ScriptWitness PlutusScript -> M Unit
usePlutusScriptWitness =
  case _ of
    ScriptValue ps -> do
      _transaction <<< _witnessSet <<< _plutusScripts
        %= pushUnique ps
    ScriptReference input action -> do
      _transaction <<< _body <<< refInputActionToLens action
        %= pushUnique input

useNativeScriptWitness :: ScriptWitness NativeScript -> M Unit
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
  :: TransactionUnspentOutput -> Maybe DatumWitness -> M Unit
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
              _datums %= pushUnique providedDatum
              _transaction <<< _witnessSet <<< _plutusData
                %= pushUnique providedDatum
        -- otherwise, fail
        Just (DatumValue providedDatum) -> do
          throwError $ IncorrectDatumHash utxo providedDatum datumHash
        -- if a reference input is provided, we just attach it.
        -- TODO: consider querying for the inline datum to check if it matches.
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

-- Ensures uniqueness
appendInput
  :: TransactionInput -> Array TransactionInput -> Array TransactionInput
appendInput a b = Set.toUnfoldable (Set.singleton a <> Set.fromFoldable b)

-- | Redeemer that hasn't yet been indexed, that tracks its purpose info
-- | that is enough to find its index given a `RedeemersContext`.
newtype UnindexedRedeemer = UnindexedRedeemer
  { datum :: PlutusData
  , purpose :: RedeemerPurpose
  }

derive instance Generic UnindexedRedeemer _
derive instance Newtype UnindexedRedeemer _
derive newtype instance Eq UnindexedRedeemer
derive newtype instance Ord UnindexedRedeemer
derive newtype instance EncodeAeson UnindexedRedeemer

instance Show UnindexedRedeemer where
  show = genericShow

-- | Ignore the value that the redeemer points to
redeemerPurposeToRedeemerTag :: RedeemerPurpose -> RedeemerTag
redeemerPurposeToRedeemerTag = case _ of
  ForSpend _ -> Spend
  ForMint _ -> Mint
  ForReward _ -> Reward
  ForCert _ -> Cert

unindexedRedeemerToRedeemer :: UnindexedRedeemer -> Redeemer
unindexedRedeemerToRedeemer (UnindexedRedeemer { datum, purpose }) =
  Redeemer
    { tag: redeemerPurposeToRedeemerTag purpose
    , "data": wrap datum
    , index: BigNum.zero
    , exUnits: ExUnits.empty
    }
