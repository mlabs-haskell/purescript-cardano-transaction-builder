# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [v2.0.1](#v201)
  - [Changed](#changed)
  - [Removed](#removed)
- [v2.0.0](#v200)
  - [Added](#added)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

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
