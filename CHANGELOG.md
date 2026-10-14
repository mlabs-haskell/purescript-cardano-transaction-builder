# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [[Unreleased]](#unreleased)
  - [Changed](#changed)
  - [Removed](#removed)
- [v3.0.0](#v300)
  - [Changed](#changed-1)
- [v2.0.1](#v201)
  - [Changed](#changed-2)
  - [Removed](#removed-1)
- [v2.0.0](#v200)
  - [Added](#added)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## [Unreleased]

### Changed

- Spending a datum-less UTxO is now permitted at the builder level:
  `useDatumWitnessForUtxo` no longer errors when the resolved output has no
  datum. This unblocks spending V3 script outputs without a datum
  ([CIP-69](https://cips.cardano.org/cip/CIP-0069)). V1/V2 misuse of the same
  shape is still caught, but by the ledger at phase-1.
  ([#10](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/10))

### Removed

- `WrongSpendWitnessType` constructor of `TxBuildError`. The builder no
  longer produces this error - see the corresponding entry under
  _Changed_. ([#10](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/10))

## v3.0.0

### Changed

- Replaced cardano-serialization-lib (CSL) with [cardano-data-lite (CDL)](https://github.com/mlabs-haskell/purescript-cardano-data-lite) ([#7](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/7))

## v2.0.1

### Changed

- Updated to CSL v12.0.0

### Removed

- `UnableToAddMints` error, because `Mint` type is now a semigroup in `cardano-types`.

## v2.0.0

### Added

- New transaction builder steps: `SubmitProposal` for attaching governance
proposals and `SubmitVotingProcedure` for voting.
([#3](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/3))

- New transaction builder errors: `UnneededSpoVoteWitness` and
`UnneededProposalPolicyWitness`.
([#3](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/3))

- Support for Conway era certificates.
([#3](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/3))
