# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/)
and we follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [[Unreleased]](#unreleased)
  - [Added](#added)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## [Unreleased]

### Added

- New transaction builder steps: `SubmitProposal` for attaching governance
proposals and `SubmitVotingProcedure` for voting.
([#3](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/3))

- New transaction builder errors: `UnneededSpoVoteWitness` and
`UnneededProposalPolicyWitness`.
([#3](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/3))

- Support for Conway era certificates.
([#3](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/3))
