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
proposals and `SubmitVotingProcedure` for voting. Note that there is currently
no support for providing a credential witness for the optional constitution
guardrails script, which restricts certain proposal types as outlined in
[CIP-1694 Specification](https://github.com/cardano-foundation/CIPs/blob/b81611632f9dcea0b87d7d96cf673a720c77e929/CIP-1694/README.md#guardrails-script).
([#3](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/3))

- Support for Conway era certificates.
([#3](https://github.com/mlabs-haskell/purescript-cardano-transaction-builder/pull/3))
