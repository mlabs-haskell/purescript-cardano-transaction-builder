module Test.Main where

import Prelude

import Data.Lens ((.~), (<>~))
import Cardano.AsCbor (decodeCbor)
import Cardano.Transaction.Builder
  ( ExpectedWitnessType(..)
  , OutputWitness(..)
  , RefInputAction(..)
  , ScriptWitness(..)
  , TransactionBuilderStep(..)
  , TxBuildError(..)
  , buildTransaction
  )
import Cardano.Transaction.Edit (editTransaction, editTransactionSafe)
import Cardano.Types
  ( Redeemer(Redeemer)
  , RedeemerTag(Spend)
  , Transaction
  , TransactionInput(TransactionInput)
  , TransactionOutput(TransactionOutput)
  , TransactionUnspentOutput(TransactionUnspentOutput)
  , _outputs
  , _redeemers
  , _witnessSet
  )
import Cardano.Types.Address (Address(..))
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Coin (Coin(..))
import Cardano.Types.Credential (Credential(..))
import Cardano.Types.ExUnits as ExUnits
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.NativeScript (NativeScript(..))
import Cardano.Types.NetworkId (NetworkId(..))
import Cardano.Types.PlutusData (PlutusData(..))
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction (_body)
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionBody (_inputs)
import Cardano.Types.Value (Value(..))
import Data.ByteArray (byteArrayFromIntArrayUnsafe, hexToByteArrayUnsafe)
import Data.Either (Either(Left, Right))
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
    oneInput = Transaction.empty
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
        tx' = Transaction.empty
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
        tx = Transaction.empty
          # _witnessSet <<< _redeemers .~
              [ Redeemer
                  { index: BigNum.one
                  , data: RedeemerDatum.unit
                  , tag: Spend
                  , exUnits: ExUnits.empty
                  }
              ]
          # _body <<< _inputs .~ [ input0, input1, input2 ]
        tx' = Transaction.empty
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
        `shouldEqual` Just tx'
    test "remove two inputs with redeemers, before and after" do
      let
        tx = Transaction.empty
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
        tx' = Transaction.empty
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
        `shouldEqual` Just tx'
    test "remove input & redeemer, add another input & redeemer" do
      let
        tx = Transaction.empty
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
        tx' = Transaction.empty
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
        `shouldEqual` Just tx'

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
      Transaction.empty # _body <<< _inputs .~ [ input1 ]
    testBuilderSteps "PKH output x2 -> 1"
      [ SpendOutput pkhUtxo Nothing, SpendOutput pkhUtxo Nothing ] $
      Transaction.empty # _body <<< _inputs .~ [ input1 ]
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
          buildTransaction TestnetId
          [ SpendOutput pkhUtxo Nothing ]
      result `shouldEqual` Left WrongNetworkId
  group "Pay" do
    testBuilderSteps "#1" [ Pay pkhOutput ] $
      Transaction.empty # _body <<< _outputs .~ [ pkhOutput ]
  group "MintAsset" do
    testBuilderSteps "#1" [ Pay pkhOutput ] $
      Transaction.empty # _body <<< _outputs .~ [ pkhOutput ]

testBuilderStepsFail
  :: String
  -> Array TransactionBuilderStep
  -> TxBuildError
  -> TestPlanM (Aff Unit) Unit
testBuilderStepsFail label steps err = test label do
  let
    result = buildTransaction MainnetId steps
  result `shouldEqual` Left err

testBuilderSteps
  :: String
  -> Array TransactionBuilderStep
  -> Transaction
  -> TestPlanM (Aff Unit) Unit
testBuilderSteps label steps expected = test label do
  let
    result = buildTransaction MainnetId steps
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
          , stakeCredential: wrap $ PubKeyHashCredential $ unsafePartial
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
          }
      , amount: Value (Coin (BigNum.fromInt 5000000)) MultiAsset.empty
      , datum: Nothing
      , scriptRef: Nothing
      }
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
