# purescript-cardano-transaction-builder [![cardano-purescript](https://img.shields.io/badge/cardano--purescript?logo=cardano&logoColor=white&label=cardano-purescript&labelColor=blue&color=blue)](https://github.com/klntsky/cardano-purescript)

This library provides a declarative interface for transaction building (`Cardano.Transaction.Builder`) and a functional interface for editing a transaction (`Cardano.Transaction.Edit`) without having to maintain correct redeemer indices.

# Transaction builder

Builder is a declarative API that accepts a high-level description of a transaction, written in a special SDL, and transforms it into actions that materialize the transaction.

Here's a short overview of the interface to give an impression of how it works:

```purescript
buildTransaction
  :: Array TransactionBuilderStep
  -> Either TxBuildError Transaction

data TransactionBuilderStep
  = SpendOutput TransactionUnspentOutput (Maybe OutputWitness)
  | Pay TransactionOutput
  | MintAsset ScriptHash AssetName Int.Int CredentialWitness
  | IssueCertificate Certificate (Maybe CredentialWitness)
  | WithdrawRewards StakeCredential Coin (Maybe CredentialWitness)
  | SubmitProposal VotingProposal (Maybe CredentialWitness)
  | SubmitVotingProcedure Voter (Map GovernanceActionId VotingProcedure)
      (Maybe CredentialWitness)

data OutputWitness
  = NativeScriptOutput (ScriptWitness NativeScript)
  | PlutusScriptOutput (ScriptWitness PlutusScript) RedeemerDatum
      (Maybe DatumWitness)

data ScriptWitness a
  = ScriptValue a
  | ScriptReference TransactionInput RefInputAction

data RefInputAction
  = ReferenceInput
  | SpendInput

data DatumWitness
  = DatumValue PlutusData
  | DatumReference TransactionInput RefInputAction
```

The interface guides the user naturally, requiring them to provide everything that is needed to perform a desired action on-chain.

A returned transaction is still *unbalanced*, which means that the sum of inputs does not equal the sum of outputs. Transaction balancing is a non-trivial process, consider using [`cardano-transaction-lib`](https://github.com/Plutonomicon/cardano-transaction-lib/) for that.

To modify an existing transaction by supplying additional `TransactionBuilderStep`s, `modifyTransaction` can be used:

```purescript
modifyTransaction
  :: Transaction
  -> Array TransactionBuilderStep
  -> Either TxBuildError Transaction
```

# Transaction editor

Transaction editor is another interface for transaction building.

According to the ledger spec, redeemers that are stored in a
`TransactionWitnessSet`, contain pointers to various transaction parts.

The pointers are just numbers corresponding to indices in arrays.

For example, a redeemer for spending a UTxO locked at a script address
contains an index of the corresponding input in the
list of transaction body inputs.

This is not very convenient for developers, because of the need to
keep the indices correct while modifying the transaction.

For instance, if a new mint is added, all mint redeemer indices may have to
be updated.

`Cardano.Transaction.Edit` module automates tracking of redeemers and provides a simple interface to modify a transaction safely:

```purescript
editTransaction :: (Transaction -> Transaction) -> (Transaction -> Transaction)
```

There's also a safer variant that fails if any of the redeemers (before or after tx modification) contain invalid pointers:

```purescript
editTransactionSafe :: (Transaction -> Transaction) -> (Transaction -> Maybe Transaction)
```
