module Test.Main where

import Prelude

import Cardano.AsCbor (class AsCbor, decodeCbor, encodeCbor)
import Cardano.Transaction.Builder (ExpectedWitnessType(..), OutputWitness(..), RefInputAction(..), ScriptWitness(..), TransactionBuilderStep(..), TxBuildError(..), buildTransaction)
import Cardano.Types (Transaction(..), TransactionInput(..), TransactionOutput(..), TransactionWitnessSet)
import Cardano.Types.Address (Address(..))
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Coin (Coin(..))
import Cardano.Types.Credential (Credential(..))
import Cardano.Types.MultiAsset as MultiAsset
import Cardano.Types.NativeScript (NativeScript(..))
import Cardano.Types.NetworkId (NetworkId(..))
import Cardano.Types.RedeemerDatum as RedeemerDatum
import Cardano.Types.Transaction (_body)
import Cardano.Types.Transaction as Transaction
import Cardano.Types.TransactionBody (_inputs)
import Cardano.Types.TransactionUnspentOutput (TransactionUnspentOutput(..))
import Cardano.Types.Value (Value(..))
import Data.ByteArray (ByteArray, byteArrayFromIntArrayUnsafe)
import Data.Either (Either(..))
import Data.Lens ((.~))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Milliseconds(Milliseconds))
import Data.UInt as UInt
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Mote (group, test)
import Mote.TestPlanM (TestPlanM, interpretWithConfig)
import Partial.Unsafe (unsafePartial)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Runner (defaultConfig)
import Type.Proxy (Proxy(Proxy))

main :: Effect Unit
main = launchAff_ do
  interpretWithConfig
    defaultConfig { timeout = Just $ Milliseconds 30_000.0, exit = true }
    suite

suite :: TestPlanM (Aff Unit) Unit
suite = do
  let
    pkhUtxo =
      TransactionUnspentOutput
        { input
        , output: pkhOutput
        }
    nsWitness = NativeScriptOutput (ScriptValue $ ScriptAll [])
    plutusScriptRefWitness =
      PlutusScriptOutput (ScriptReference input SpendInput) RedeemerDatum.unit Nothing
  group "SpendOutput" do
    testBuilderSteps "PKH output" [ SpendOutput pkhUtxo Nothing ] $
      Transaction.empty # _body <<< _inputs .~ [ input ]
    testBuilderStepsFail "PKH output with wrong witness"
      [ SpendOutput pkhUtxo (Just nsWitness) ] $
      WrongOutputType ScriptHashWitness pkhUtxo
    testBuilderStepsFail "PKH output with wrong witness"
      [ SpendOutput pkhUtxo (Just nsWitness) ] $
      WrongOutputType ScriptHashWitness pkhUtxo
    testBuilderStepsFail "PKH output with wrong witness #2"
      [ SpendOutput pkhUtxo (Just plutusScriptRefWitness) ] $
      WrongOutputType ScriptHashWitness pkhUtxo

testBuilderStepsFail
  :: String
  -> Array TransactionBuilderStep
  -> TxBuildError
  -> TestPlanM (Aff Unit) Unit
testBuilderStepsFail label steps err = test label do
  let
    result = buildTransaction MainnetId Map.empty steps
  Left err `shouldEqual` map _.transaction result

testBuilderSteps
  :: String
  -> Array TransactionBuilderStep
  -> Transaction
  -> TestPlanM (Aff Unit) Unit
testBuilderSteps label steps expected = test label do
  let
    result = buildTransaction MainnetId Map.empty steps
  Right expected `shouldEqual` map _.transaction result

input :: TransactionInput
input = TransactionInput
  { index: UInt.fromInt 0
  , transactionId: unsafePartial $ fromJust $ decodeCbor $ wrap
      ( byteArrayFromIntArrayUnsafe
          [ 198
          , 181
          , 74
          , 163
          , 1
          , 136
          , 122
          , 243
          , 144
          , 189
          , 52
          , 73
          , 131
          , 62
          , 76
          , 214
          , 111
          , 246
          , 27
          , 94
          , 104
          , 177
          , 247
          , 124
          , 132
          , 168
          , 192
          , 135
          , 59
          , 119
          , 111
          , 249
          ]
      )
  }

pkhOutput :: TransactionOutput
pkhOutput =
  ( TransactionOutput
      { address: BaseAddress
          { networkId: TestnetId
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

-- utxo1 =  TransactionUnspentOutput
--     { input
--     , output
--     }
