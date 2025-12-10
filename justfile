# shellcheck shell=bash

format:
   #!/usr/bin/env bash
   fourmolu -i src app test cardano-read-ledger/src cardano-read-ledger/test
   fourmolu -i src app test cardano-read-ledger/src cardano-read-ledger/test
   fourmolu -i src app test cardano-read-ledger/src cardano-read-ledger/test
   cabal-fmt -i cardano-n2n-client.cabal  cardano-read-ledger/cardano-read-ledger.cabal
   # shellcheck disable=SC2035
   nixfmt *.nix
   nixfmt nix/*.nix

hlint:
  #!/usr/bin/env bash
  hlint app src test

build:
    #!/usr/bin/env bash
    cabal build all --enable-tests --enable-benchmarks

test:
  #!/usr/bin/env bash
  cabal test --enable-tests --enable-benchmarks

CI:
  #!/usr/bin/env bash
  just format
  just hlint
  just build
  just test

watch-docs:
  #!/usr/bin/env bash
  mkdocs serve

deploy-docs:
  #!/usr/bin/env bash
  mkdocs gh-deploy --clean