#!/bin/bash

cabal run -- token-pab \
  --config testnet/pab-config.yml webserver \
  --passphrase SandrosPassphrase
