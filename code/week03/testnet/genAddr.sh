cd /Users/sandro/developing/learning/plutus/cardano-node
./cardano-cli address key-gen --verification-key-file 01.vkey --signing-key-file 01.skey
./cardano-cli address build --payment-verification-key-file 01.vkey --testnet-magic 1097911063 --out-file 01.addr
