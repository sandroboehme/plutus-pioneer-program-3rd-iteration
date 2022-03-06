4. [CIP 25 - NFT Metadata Standard](https://cips.cardano.org/cips/cip25/)
   1. [GitHub](https://github.com/cardano-foundation/CIPs/blob/master/CIP-0025/README.md)
5. NFTs on the testnet:
    1. https://testnet.adatools.io/nft
    2. https://testnet.adatools.io/tokens
6. You can play around with the metadata file and build an example with this website:
   1. https://pool.pm/test/metadata
7. NFTs on the mainnet: https://pool.pm/tokens
8. The the new UTxO with spendable ADA: `./query-key1.sh`
9. Use it to create the policy script file
   1. E.g. `cabal exec token-policy -- testnet/token.plutus b891e9b26099bfc4165e2004572e0816e63c2f96798b609301e9fcb144b916e8#0 1 kaffee`
10. Get the policy id
    1. `cardano-cli transaction policyid --script-file testnet/token.plutus`
       1. Result in my example: `3eab9953ed4448bde21458545d0b82330026165fee13147baf742345`
11. Upload the image to IPFS with a free service
    1. https://nft.storage/
12. Build up the metadata file  and 
     2. 
      ```json
       {
          "721": {
             "<policy-id>": {
                "<token-name>": {
                   "name": "a name",
                   "image": "ipfs hash",
                   "mediaType": "<mime-type>",
                    "...": "..."
                }
             },
             "version": "1.0"
          }
       }
       ```
    2. Enter the data (make sure you only use the ipfs hash in the image without adding the "ipfs://" scheme) 
       ```json
         {
            "721": {
               "3eab9953ed4448bde21458545d0b82330026165fee13147baf742345": {
                  "kaffee": {
                     "name": "kaffee",
                     "image": "bafkreidaa3wkpmwx7ujrp7kyoyyzkbpdkkyq4mrgb6aa43kkt34tytpiey",
                     "mediaType": "image/jpeg"
                  }
               },
               "version": "1.0"
            }
         }
         ```
    3. Change `./mint-token-cli.sh`
       1. Add `metadataJsonFile=$6`
       3. Change `policyFile=testnet/token.plutus` to the right file name
       4. Add `--metadata-json-file $metadataJsonFile \`
       5. Call the cli script
          1. `./mint-token-cli.sh b891e9b26099bfc4165e2004572e0816e63c2f96798b609301e9fcb144b916e8#0 1 kaffee testnet/01.addr testnet/01.skey /Users/sandro/.../kaffeeNFTmetadata.json`
    4. Check after some time if the NFT is available at https://testnet.adatools.io/nft
    
