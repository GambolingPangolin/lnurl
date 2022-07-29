# lnurl-payment-tool

_Warning:_ This is alpha software.  We welcome tester feedback and patches.

This tool exposes two commands `withdraw` and `pay` each of which take an LNURL string (with optional `lightning:` prefix).  Each tool steps the user through the workflow, asking the user to supply amounts and pay or generate invoices via their LN wallet.  Privacy conscious users may couple this tool with tor.  For example:

```console
$ torify lnurl-payment-tool withdraw $LNURL
```

## Installation or development

You will need haskell tooling.  Install `cabal` using your favorite package manager or [`ghcup`][1].  Add the `cabal` bin folder to your path, e.g. `~/.cabal/bin`.  Clone this repo.  Then from the repo root, run

```console
cabal build all && cabal install
```

[1]: https://www.haskell.org/ghcup/
