module Test.Main where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Transaction.Builder
  ( CredentialWitness(PlutusScriptCredential)
  , ExpectedWitnessType(ScriptHashWitness)
  , OutputWitness(PlutusScriptOutput, NativeScriptOutput)
  , RefInputAction(SpendInput)
  , ScriptWitness(ScriptValue, ScriptReference)
  , TransactionBuilderStep(IssueCertificate, Pay, SpendOutput)
  , TxBuildError
      ( UnneededDeregisterWitness
      , WrongNetworkId
      , IncorrectScriptHash
      , WrongOutputType
      )
  , buildTransaction
  , modifyTransaction
  )
import Cardano.Transaction.Edit (editTransaction, editTransactionSafe)
import Cardano.Types
  ( Address(BaseAddress)
  , NetworkId(MainnetId, TestnetId)
  , RedeemerTag(Cert, Spend)
  , Certificate(StakeDeregistration)
  , Coin(Coin)
  , Credential(PubKeyHashCredential, ScriptHashCredential)
  , Redeemer(Redeemer)
  , Transaction
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , TransactionUnspentOutput(TransactionUnspentOutput)
  , PlutusScript
  , PlutusData(List, Map)
  , NativeScript(ScriptAll)
  , Value(Value)
  , _address
  , _certs
  , _networkId
  , _output
  , _outputs
  , _plutusScripts
  , _redeemers
  , _witnessSet
  , _inputs
  , _body
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.ExUnits as ExUnits
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction as Transaction
import Data.ByteArray (byteArrayFromIntArrayUnsafe, hexToByteArrayUnsafe)
import Data.Either (Either(Left, Right))
import Data.Lens ((.~), (<>~), (^.))
import Data.Maybe (Maybe(Just, Nothing), fromJust)
import Data.Newtype (wrap)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Mote (group, test)
import Mote.TestPlanM (TestPlanM, interpretWithConfig)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (defaultConfig)

main :: Effect Unit
main = launchAff_ do
  interpretWithConfig
    defaultConfig { timeout = Just $ Milliseconds 30_000.0, exit = true }
    suite

suite :: TestPlanM (Aff Unit) Unit
suite = do
  editorTests
  builderTests

editorTests :: TestPlanM (Aff Unit) Unit
editorTests = group "Cardano.Transaction.Edit" do
  let
    oneInput = anyNetworkTx
      # _witnessSet <<< _redeemers .~
          [ Redeemer
              { index: BigNum.zero
              , data: RedeemerDatum.unit
              , tag: Spend
              , exUnits: ExUnits.empty
              }
          ]
      # _body <<< _inputs .~
          [ input1 ]
  group "editTransaction" do
    test "do nothing" do
      editTransaction identity oneInput `shouldEqual` oneInput
    test "attach one input to the end" do
      let
        tx' = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.zero
                  , data: RedeemerDatum.unit
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~
              [ input1, input2 ]
      editTransaction (_body <<< _inputs <>~ [ input2 ]) oneInput
        `shouldEqual` tx'
    test "remove two inputs, before and after" do
      let
        tx = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.one
                  , data: RedeemerDatum.unit
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~ [ input0, input1, input2 ]
        tx' = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.zero
                  , data: RedeemerDatum.unit
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~
              [ input1 ]
      editTransactionSafe (_body <<< _inputs .~ [ input1 ]) tx
        `shouldEqual` pure tx'
    test "remove two inputs with redeemers, before and after" do
      let
        tx = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.zero
                  , data: RedeemerDatum.unit
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              , Redeemer
                  { index: BigNum.one
                  , data: wrap $ List []
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              , Redeemer
                  { index: BigNum.fromInt 2
                  , data: wrap $ Map []
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~ [ input0, input1, input2 ]
        tx' = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.zero
                  , data: wrap $ List []
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~
              [ input1 ]
      editTransactionSafe (_body <<< _inputs .~ [ input1 ]) tx
        `shouldEqual` pure tx'
    test "remove input & redeemer, add another input & redeemer" do
      let
        tx = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.zero
                  , data: RedeemerDatum.unit
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              , Redeemer
                  { index: BigNum.one
                  , data: wrap $ Map []
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~ [ input1, input2 ]
        tx' = anyNetworkTx
          # _witnessSet <<< _redeemers .~
              -- order is swapped because of `nub`...
              [ Redeemer
                  { index: BigNum.one
                  , data: wrap $ Map []
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              , Redeemer
                  { index: BigNum.zero
                  , data: wrap $ List []
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~
              [ input0, input2 ]
      editTransactionSafe
        ( (_body <<< _inputs .~ [ input0, input2 ]) >>>
            ( _witnessSet <<< _redeemers <>~
                [ Redeemer
                    { index: BigNum.zero
                    , data: wrap $ List []
                    , tag: Spend
                    , exUnits: ExUnits.empty
                    }
                ]
            )
        )
        tx
        `shouldEqual` pure tx'

builderTests :: TestPlanM (Aff Unit) Unit
builderTests = group "Cardano.Transaction.Builder" do
  let
    pkhUtxo =
      TransactionUnspentOutput
        { input: input1
        , output: pkhOutput
        }
    nsWitness = NativeScriptOutput (ScriptValue $ ScriptAll [])
    plutusScriptRefWitness =
      PlutusScriptOutput (ScriptReference input1 SpendInput) RedeemerDatum.unit Nothing
  group "SpendOutput" do
    testBuilderSteps "PKH output" [ SpendOutput pkhUtxo Nothing ] $
      anyNetworkTx # _body <<< _inputs .~ [ input1 ]
    testBuilderSteps "PKH output x2 -> 1"
      [ SpendOutput pkhUtxo Nothing, SpendOutput pkhUtxo Nothing ] $
      anyNetworkTx # _body <<< _inputs .~ [ input1 ]
    testBuilderStepsFail "PKH output with wrong witness"
      [ SpendOutput pkhUtxo (Just nsWitness) ] $
      WrongOutputType ScriptHashWitness pkhUtxo
    testBuilderStepsFail "PKH output with wrong witness"
      [ SpendOutput pkhUtxo (Just nsWitness) ] $
      WrongOutputType ScriptHashWitness pkhUtxo
    testBuilderStepsFail "PKH output with wrong witness #2"
      [ SpendOutput pkhUtxo (Just plutusScriptRefWitness) ] $
      WrongOutputType ScriptHashWitness pkhUtxo
    test "PKH output with wrong NetworkId" do
      let
        result =
          modifyTransaction testnetTransaction
            [ SpendOutput pkhUtxo Nothing ]
      result `shouldEqual`
        Left (WrongNetworkId $ pkhUtxo ^. _output <<< _address)
  group "Pay" do
    testBuilderSteps "#1" [ Pay pkhOutput ] $
      anyNetworkTx # _body <<< _outputs .~ [ pkhOutput ]
  group "MintAsset" do
    testBuilderSteps "#1" [ Pay pkhOutput ] $
      anyNetworkTx # _body <<< _outputs .~ [ pkhOutput ]
  group "Deregister" do
    testBuilderSteps "Deregister script"
      [ IssueCertificate
          (StakeDeregistration (wrap $ ScriptHashCredential $ PlutusScript.hash script1))
          $ Just
          $ PlutusScriptCredential (ScriptValue script1) RedeemerDatum.unit
      ] $
      anyNetworkTx
        # _witnessSet <<< _plutusScripts .~ [ script1 ]
        # _witnessSet <<< _redeemers .~
            [ Redeemer
                { exUnits: ExUnits.empty
                , tag: Cert
                , index: BigNum.zero
                , data: RedeemerDatum.unit
                }
            ]
        # _body <<< _certs .~
            [ StakeDeregistration $ wrap $ ScriptHashCredential $
                PlutusScript.hash script1
            ]
    do
      let
        witness =
          PlutusScriptCredential (ScriptValue script1) RedeemerDatum.unit
      testBuilderStepsFail "deregistering stake credential with unneeded witness fails"
        [ IssueCertificate (StakeDeregistration $ wrap $ pubKeyHashCredential1) $ Just witness ] $
        UnneededDeregisterWitness (wrap $ pubKeyHashCredential1) witness
    testBuilderStepsFail "deregistering stake credential with wrong witness fails"
      [ IssueCertificate (StakeDeregistration $ wrap $ ScriptHashCredential $ PlutusScript.hash script2)
          $ Just
          $ PlutusScriptCredential (ScriptValue script1) RedeemerDatum.unit
      ] $
      IncorrectScriptHash (Right script1) (PlutusScript.hash script2)

testBuilderStepsFail
  :: String
  -> Array TransactionBuilderStep
  -> TxBuildError
  -> TestPlanM (Aff Unit) Unit
testBuilderStepsFail label steps err = test label do
  let
    result = buildTransaction steps
  result `shouldEqual` Left err

testBuilderSteps
  :: String
  -> Array TransactionBuilderStep
  -> Transaction
  -> TestPlanM (Aff Unit) Unit
testBuilderSteps label steps expected = test label do
  let
    result = buildTransaction steps
  result `shouldEqual` Right expected

pkhOutput :: TransactionOutput
pkhOutput =
  ( TransactionOutput
      { address: BaseAddress
          { networkId: MainnetId
          , paymentCredential: wrap $ PubKeyHashCredential $ unsafePartial
              $ fromJust
              $ decodeCbor
              $ wrap
              $
                byteArrayFromIntArrayUnsafe
                  [ 243
                  , 63
                  , 250
                  , 132
                  , 253
                  , 242
                  , 10
                  , 0
                  , 52
                  , 67
                  , 165
                  , 226
                  , 118
                  , 142
                  , 18
                  , 233
                  , 45
                  , 179
                  , 21
                  , 53
                  , 220
                  , 166
                  , 32
                  , 136
                  , 177
                  , 83
                  , 223
                  , 36
                  ]
          , stakeCredential: wrap pubKeyHashCredential1
          }
      , amount: Value (Coin (BigNum.fromInt 5000000)) MultiAsset.empty
      , datum: Nothing
      , scriptRef: Nothing
      }
  )

pubKeyHashCredential1 :: Credential
pubKeyHashCredential1 =
  PubKeyHashCredential $ unsafePartial
    $ fromJust
    $ decodeCbor
    $ wrap
        ( byteArrayFromIntArrayUnsafe
            [ 57
            , 3
            , 16
            , 58
            , 231
            , 6
            , 129
            , 67
            , 155
            , 84
            , 118
            , 254
            , 245
            , 159
            , 67
            , 155
            , 139
            , 200
            , 109
            , 132
            , 191
            , 178
            , 211
            , 118
            , 252
            , 63
            , 86
            , 23
            ]
        )

mkTransactionInput :: String -> Int -> TransactionInput
mkTransactionInput txId ix =
  TransactionInput
    { transactionId: unsafePartial $ fromJust $ decodeCbor $ wrap $
        hexToByteArrayUnsafe txId
    , index: UInt.fromInt ix
    }

input0 :: TransactionInput
input0 = mkTransactionInput "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996" 0

input1 :: TransactionInput
input1 = mkTransactionInput "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996" 1

input2 :: TransactionInput
input2 = mkTransactionInput "5d677265fa5bb21ce6d8c7502aca70b9316d10e958611f3c6b758f65ad959996" 2

script1 :: PlutusScript
script1 = unsafePartial $ fromJust $ decodeCbor $ wrap $ hexToByteArrayUnsafe "4e4d01000033222220051200120011"

script2 :: PlutusScript
script2 = unsafePartial $ fromJust $ decodeCbor $ wrap $ hexToByteArrayUnsafe "4e4d01000033222220051200120012"

anyNetworkTx :: Transaction
anyNetworkTx = Transaction.empty

testnetTransaction :: Transaction
testnetTransaction = Transaction.empty # _body <<< _networkId .~ Just TestnetId
