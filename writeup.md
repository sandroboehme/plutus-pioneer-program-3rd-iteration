# Overview over the Plutus Pioneer Program 3rd iteration
This is a writeup about the Plutus Pioneer Program lectures. I use it to be able to quickly reread or review important or more complex topics. As ymmv about what is important or complex you might miss some things. If you want you can add them here as well by providing pull requests. 
0. YouTube video shortcuts
   1. Sometimes it's quite convenient, to speed up the playback to safe time because one can understand it at a faster pace.
   2. At the other hand - sometimes one would like to type while Lars is speaking. Then one might want to slow down the video to keep up the typing pace with Lars' speaking pace.
   3. With QWERTY keyboards it is no problem to change the playback speed.
   4. With QWERTZ keyboards it doesn't work unfortunately. In that case you need a specific plugin. E.g. the one linked below. But there are others as well.
       * Shortcut overview: `?`
       * 10 sec forward / backward = `f` / `j`
       * 5 sec forward / backward = cursor left / cursor right
       * QWERTY: faster / slower = `>` / `<`
       * QWERTZ: faster / slower doesn't work out of the box
         * Install [YouTube Playback Speed Control for Chrome](chrome-extension://hdannnflhlmdablckfkjpleikpphncik/src/option/options.html)
         * Works not only for YouTube
         * faster / slower = `+` / `-` (or setup custom key mappings
        
1. EUTxO Model
   1. [Intro](https://www.youtube.com/watch?v=X80uNXenWF4&list=PLNEK_Ejlx3x2nLM4fAck2JS6KhFQlXq2N)
   2. [EUTxO Model](https://www.youtube.com/watch?v=bfofA4MM0QE&list=PLNEK_Ejlx3x2nLM4fAck2JS6KhFQlXq2N&index=2)
   3. [Building the Example Code](https://www.youtube.com/watch?v=zPaDp4R9X7o&list=PLNEK_Ejlx3x2nLM4fAck2JS6KhFQlXq2N&index=3)
      1. [Haddock Documentation](https://youtu.be/zPaDp4R9X7o?t=351)
2. [Low Level, Untyped Validation Scripts](https://www.youtube.com/watch?v=xgnmMl-eIIM&list=PLNEK_Ejlx3x0mhPmOjPSHZPtTFpfJo3Nd&index=2)
   1. UTxO model
      1. A Tx can consume a UTxO as an input if it includes the signature of that public key address
   2. Extended UTxO
      1. Allows to have not only public key addresses but also script addresses that can run arbitrary logic
      2. When the Tx is validated, the node will run the script and check if it's valid or not.
      3. Also allows to have 
         1. Redeemers on the input side and
         2. Datum on the output side of the TX
   3. [Plutus script data type](https://youtu.be/xgnmMl-eIIM?t=307)
      1. Redeemers, Datum and Context all have the same generic low level `BuildinData` data type.
      2. It can be converted to higher level data types but it comes at a cost and some projects have choosen not to do that.
      3. The `Data` type is similar to a JSON format and can be used to convert to `BuildinData` 
      4. [`ByteString` `B` constructor and overloaded Strings with `:set -XOverloadedStrings`](https://youtu.be/xgnmMl-eIIM?t=509)
         1. Literal Strings are just lists of characters
         2. With this extension one can use literal Strings for other Stringlike types as well and one of those types is the `ByteString` type.
         3. E.g. `B "Haskell"` returns `"B Haskell" :: Data`
   4. [Writing the validator - `mkValidator`](https://youtu.be/xgnmMl-eIIM?t=600)
      1. Return type is unit. In case it's invalid an error is thrown otherwise nothing happens.
      2. A script address is more or less the hash of the validator
   5. [Writing the validator - `validator`](https://youtu.be/xgnmMl-eIIM?t=1028)
      1. Works like a makro.
      2. It converts `mkValidator` expression to an abstract syntax tree using the oxford brackets `||`
      3. Compiles it to the Plutus Core syntax (-tree) using `PlutusTx.compile`.
      4. `$$` takes the Plutus Core syntax tree and inserts (splices) it into the source code at that point as a Plutus Core expression.
      5. The `mkValidatorScript` function uses that as a parameter and turns it into an actual validator.
      6. [`{-# INLINABLE mkValidator #-}`](https://youtu.be/xgnmMl-eIIM?t=1555) pragma allows to reuse helper or library function also in the off-chain code by seperating the definitions and not inline everything.
         1. Everything that belongs to the on-chain code needs to be inlineable.
   6. [Off-chain part](https://youtu.be/xgnmMl-eIIM?t=1900)
      1. `mustPayToOtherScript` is a transaction contstraint
         1. `valHash` is the hash of the "other script"
         2. next param: empty datum
         3. last param the actual ada value
         4. `grab`
            1. If you want to consume UTxO sitting at a script address then the spending transaction needs to provide the actual validator code whereas the producing transaction only has to provide the hash.
         5. [```
            endpoints :: Contract () GiftSchema Text ()
            endpoints = awaitPromise (give' `select` grab') >> endpoints
            where
            give' = endpoint @"give" give
            grab' = endpoint @"grab" $ const grab
            ```](https://youtu.be/xgnmMl-eIIM?t=2201)
            1. `endpoints` waits for user interaction, calles the endpoint according to the user input and waits again
   7. [Test in the playground](https://youtu.be/xgnmMl-eIIM?t=2297)
   8. [Burn](https://youtu.be/xgnmMl-eIIM?t=2623)
      1. Prelude name clashes
3. [High Level Typed Validation Scripts](https://www.youtube.com/watch?v=HoB_PqeZPNc&list=PLNEK_Ejlx3x0mhPmOjPSHZPtTFpfJo3Nd&index=3)
4. [Homework](https://www.youtube.com/watch?v=_r-EpXzQGKo&list=PLNEK_Ejlx3x0mhPmOjPSHZPtTFpfJo3Nd&index=6)
   1. Homework 1
   2. [Homework 2](https://youtu.be/_r-EpXzQGKo?t=65)
5. [Script Contexts](https://www.youtube.com/watch?v=B66xLrGXwmw)
   1. Doku: haddock
   2. Purposes
      1. `Spending` the script output
      2. `Minting` a new native token
      3. `Rewarding` for staking ADA
      4. `Certifying` e.g. for delegation certificates
   3. TxInfo
      1. Describes the spending transaction
      2. `txInfoInputs`, `txInfoOutPuts` 
      3. `txInfoFee` - Transaction fee
      4. `txInfoForge` - forged (positive) or burned (negative) native tokens
      5. `txInfoDCert` - certificates e.g. for staking
      6. `tInfoWdrl` - staking withdrawals from rewards
      7. `txInfoValidRange` - the time range this transaction will be valid
      8. `txInfoSignatories` - pub keys that have signed the transaction
      9. `txInfoData` - Dict from DatumHash to Datum to include the full Datum. Spending tx'es need to include the datum of the script output whereas producing transactions that send money to a script address and have an output at that script address only have to include the hash and can optionally include the full datum.
      10. `txInfoId` - the id of this transaction
6. [Handling time - txInfoValidRange](https://www.youtube.com/watch?v=mf06ll-4j2w)
   1. Validation fails already on wallet level
   2. How can the validation of the time range be deterministic if it can succeed in the wallet but not potentially much later on-chain?
   3. We can always run the script under exactly the same conditions in the wallet already.
      1. ==>`txInfoValidRange` of type `POSIXTimeRange`
         1. The transaction is valid this the specified time range.
         2. This time range is checked on the node in the "pre-checks" before the validation script runs.
         3. The node checks the current time and checks if the time range is within `txInfoValidRange`. If it fails, then validation fails immediately without running the validator scripts.
         4. The validator script can assume that the current time does fall into this interval as the "pre-checks" would have prevented running the script otherwise.
         5. This makes validation deterministic.
         6. It's a static piece of data attached to the transaction so the result of the validation does not depend on when it is run. 
         Weather it's run in the wallet before submission or in one of the nodes when validating the transaction.
         7. ==> The validation script is deterministic because the time rage doesn't affect it as it can be assumed to always be valid when the script is run?
         8. The trick is, to do the time check before the validation is done and then during the execution of the validator scripts we don't have to worry about it anymore and can just assume that the current time falls into this interval. Because if it wouldn't then the validation wouldn't even run in the first place because then the validation of the transaction would have failed before.
         9. While Plutus uses Posix time for the valid range, Ouroboros, the underlaying consensus protocol uses slots. This is why it is translated to slots in the final transaction.
      2. It is a type synonym for `Interval POSIXTime`
         1. http://localhost:8002/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Interval.html#t:Interval
`            Interval	 
               ivFrom :: LowerBound a	 
               ivTo :: UpperBound a`
            1. `LowerBound`:
               1. http://localhost:8002/haddock/plutus-ledger-api/html/Plutus-V1-Ledger-Interval.html#t:LowerBound
               2. `LowerBound (Extended a) Closure`
               3. See members further below
            2. `POSIXTime` is an instance of `Ord`
            3. [Members of `LowerBound`](https://youtu.be/mf06ll-4j2w?t=585)
               1. `interval` is a smart constructor of `Interval` that gives us an interval with a lower and upper bound included.
               2. `from a` = from `a` and lasts until eternety
               3. `to a` = from eternety until `a`
            4. [Playing with the intervall api](https://youtu.be/mf06ll-4j2w?t=702)
   4. When the tx gets submitted to the blockchain and validated by a node, then - before any validation scripts are run some general checks are done. E.g. 
      1. all the inputs are present 
      2. the balences add up, 
      3. the fees are included 
      4. also the time range is checked
         1. If the current time does not fall into that time range then validation fails immediately without ever running the validator scripts.
      7. By default all transaction use the infinity time range that would always be valid.
   5. The consensus protocol Ouroboros uses slots instead of posix time ==> Slot is the native measure of time in Cardano. But Plutus uses realtime.
   6. To convert between realtime and slot numbers.
   7. If the slot length would change the conversion could be wrong. To be on the safe side - don't use a longer time horizon than 36 hours in the future (or infinity). Because if the protocol parameters would change one would know it at least 36 hours in advance.
   8. [POSIXTimeRange](https://youtu.be/mf06ll-4j2w?t=525)
      1. [Repl examples](https://youtu.be/mf06ll-4j2w?t=705)
7. [Vesting Example](https://www.youtube.com/watch?v=ae7U_yKIQ0Y)
   1. The deadline will be within the valid time interval when the validation script runs. But that time might still be before the deadline: ![Deadline](./writeupImages/deadline.png)
   2. The whole interval must be after the deadline.
      1. The interval from the deadline until infinity must contain the transaction interval
         1. This way the current time will be after the deadline.
   3. [`grab`](https://youtu.be/ae7U_yKIQ0Y?t=871)
      1. query all UTxOs where the wallet is the beneficiary and where the deadline has been reached.
      2. Create a transaction that collect all of them in the same transaction
      3. In reality the transaction can only be of a limited size so not many UTxOs could be used at the same time in reality.
      4. `Constraints.mustValidateIn (from now)`
   4. [Simulation](https://youtu.be/ae7U_yKIQ0Y?t=1173)
      1. Wallet 1 gives ADA to script for wallet 2 at slot 10
      2. Wallet 1 gives ADA to script for wallet 2 at slot 20
      3. Wallet 1 gives ADA to script for wallet 3 at slot 10
      4. Wait for 11 / 21 slots
      5. Wallet 2 grabs
      6. Wallet 3 grabs
      7. Wait for 5 slots 
      8. [Retrieve the data to input in the repl](https://youtu.be/ae7U_yKIQ0Y?t=1319)
         1. Get the `getPubKeyHash`
            1. `import Wallet.Emulator`
            2. `:i Wallet` wraps a walletId
            3. For real wallets but also for mock wallets that are used in the playground
            4. `:i WalletId`
            5. `knownWallet 2` returns a `Wallet` instance of wallet 2
            6. `mockWalletPaymentPubKeyHash` returns the PaymentPubKeyHash of a mock wallet and takes a wallet as a parameter.
               1. `mockWalletPaymentPubKeyHash $ knownWallet 2`
         2. Convert slots to PosixTime
            1. `import Ledger.Time`
            2. `import Ledger.TimeSlot`
            3. `import Data.Default`
            4. `slotToBeginPOSIXTime def 10`
8. [Parameterized Contracts](https://www.youtube.com/watch?v=XqFILXV_ACM)
   1. The validator script will get a parameter compiled into it
   2. Can only be used for datalike types but cannot contain functions
9. [Deploy to the testnet](https://www.youtube.com/watch?v=ABtffZPoUqU)
   1. If a transaction fails it normally fails already when submitting it to the blockchain. But one could insist to send it anyways and if it actually fails someone needs to handle the transaction costs. For that case a colleteral is needed.
10. [Homework](https://www.youtube.com/watch?v=GGUT2O_0urQ&list=PLNEK_Ejlx3x2zxcfoVGARFExzOHwXFCCL&index=7)
    1. First part
    2. [Second part](https://youtu.be/GGUT2O_0urQ?t=367)
