# Lecture 2 Chapter Links

0. YouTube video shortcuts
    * Shortcut overview: `?`
    * 10 sec forward / backward = `f` / `j`
    * 5 sec forward / backward = cursor left / cursor right
    * QWERTY: faster / slower = `>` / `<`
    * QWERTZ: faster / slower doesn't work out of the box
      * Install [YouTube Playback Speed Control for Chrome](chrome-extension://hdannnflhlmdablckfkjpleikpphncik/src/option/options.html)
      * Works not only for YouTube
      * faster / slower = `+` / `-` (or setup custom key mappings)

1. [Low Level, Untyped Validation Scripts](https://www.youtube.com/watch?v=xgnmMl-eIIM&list=PLNEK_Ejlx3x0mhPmOjPSHZPtTFpfJo3Nd&index=2)
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
2. [High Level Typed Validation Scripts](https://www.youtube.com/watch?v=HoB_PqeZPNc&list=PLNEK_Ejlx3x0mhPmOjPSHZPtTFpfJo3Nd&index=3)
3. [Homework](https://www.youtube.com/watch?v=_r-EpXzQGKo&list=PLNEK_Ejlx3x0mhPmOjPSHZPtTFpfJo3Nd&index=6)
   1. Homework 1
   2. [Homework 2](https://youtu.be/_r-EpXzQGKo?t=65)
