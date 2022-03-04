{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Week07.EvenOdd
    ( Game (..)
    , GameChoice (..)
    , FirstParams (..)
    , SecondParams (..)
    , GameSchema
    , endpoints
    ) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Prelude              (Semigroup (..), Show (..), String)
import qualified Prelude

data Game = Game
    { gFirst          :: !PaymentPubKeyHash
    , gSecond         :: !PaymentPubKeyHash
    , gStake          :: !Integer
    , gPlayDeadline   :: !POSIXTime
    , gRevealDeadline :: !POSIXTime
    , gToken          :: !AssetClass
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)

PlutusTx.makeLift ''Game

data GameChoice = Zero | One
    deriving (Show, Generic, FromJSON, ToJSON, ToSchema, Prelude.Eq, Prelude.Ord)

instance Eq GameChoice where
    {-# INLINABLE (==) #-}
    Zero == Zero = True
    One  == One  = True
    _    == _    = False

PlutusTx.unstableMakeIsData ''GameChoice

data GameDatum = GameDatum BuiltinByteString (Maybe GameChoice)
    deriving Show

instance Eq GameDatum where
    {-# INLINABLE (==) #-}
    GameDatum bs mc == GameDatum bs' mc' = (bs == bs') && (mc == mc')

PlutusTx.unstableMakeIsData ''GameDatum

data GameRedeemer = Play GameChoice | Reveal BuiltinByteString | ClaimFirst | ClaimSecond
    deriving Show

PlutusTx.unstableMakeIsData ''GameRedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE gameDatum #-}
gameDatum :: Maybe Datum -> Maybe GameDatum
gameDatum md = do
    Datum d <- md
    PlutusTx.fromBuiltinData d

{-# INLINABLE mkGameValidator #-}
mkGameValidator :: Game -> BuiltinByteString -> BuiltinByteString -> GameDatum -> GameRedeemer -> ScriptContext -> Bool
mkGameValidator game bsZero' bsOne' dat red ctx =
    -- Conditions - see: https://youtu.be/yczHkTzDnpk?t=1420
    traceIfFalse "token missing from input" (assetClassValueOf (txOutValue ownInput) (gToken game) == 1) &&
    case (dat, red) of
        -- see: https://youtu.be/yczHkTzDnpk?t=1447
        -- First player has moved, the second player is moving now.
        (GameDatum bs Nothing, Play c) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (unPaymentPubKeyHash $ gSecond game))             &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "second player's stake missing" (lovelaces (txOutValue ownOutput) == (2 * gStake game))            &&
            traceIfFalse "wrong output datum"            (outputDatum == GameDatum bs (Just c))                             &&
            traceIfFalse "missed deadline"               (to (gPlayDeadline game) `contains` txInfoValidRange info)         &&
            traceIfFalse "token missing from output"     (assetClassValueOf (txOutValue ownOutput) (gToken game) == 1)

        -- see: https://youtu.be/yczHkTzDnpk?t=1524
        -- Both player have moved and the Alice, the first player discovers that she has won.
        (GameDatum bs (Just c), Reveal nonce) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (unPaymentPubKeyHash $ gFirst game))              &&
            traceIfFalse "commit mismatch"               (checkNonce bs nonce c)                                            &&
            traceIfFalse "missed deadline"               (to (gRevealDeadline game) `contains` txInfoValidRange info)       &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- see: https://youtu.be/yczHkTzDnpk?t=1565
        -- The first player want's her stake back because the second player hasn't moved before the deadline
        (GameDatum _ Nothing, ClaimFirst) ->
            traceIfFalse "not signed by first player"    (txSignedBy info (unPaymentPubKeyHash $ gFirst game))              &&
            traceIfFalse "too early"                     (from (1 + gPlayDeadline game) `contains` txInfoValidRange info)   &&
            traceIfFalse "first player's stake missing"  (lovelaces (txOutValue ownInput) == gStake game)                   &&
            traceIfFalse "NFT must go to first player"   nftToFirst

        -- see: https://youtu.be/yczHkTzDnpk?t=1592
        -- Both players have moved. The second player wins as the first player missed the deadline to reveal the nonce (e.g. because she lost).
        (GameDatum _ (Just _), ClaimSecond) ->
            traceIfFalse "not signed by second player"   (txSignedBy info (unPaymentPubKeyHash $ gSecond game))             &&
            traceIfFalse "too early"                     (from (1 + gRevealDeadline game) `contains` txInfoValidRange info) &&
            traceIfFalse "wrong stake"                   (lovelaces (txOutValue ownInput) == (2 * gStake game))             &&
            traceIfFalse "NFT must go to first player"   nftToFirst
        -- everything else is invalid
        _ -> False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    ownInput :: TxOut
    -- findOwnInput - the input currently being validated
    -- see https://youtu.be/yczHkTzDnpk?t=728
    ownInput = case findOwnInput ctx of
        Nothing -> traceError "game input missing"
        Just i  -> txInInfoResolved i

    -- The outputs with the same script address that is currently being validated.
    -- see: https://youtu.be/yczHkTzDnpk?t=798
    ownOutput :: TxOut
    ownOutput = case getContinuingOutputs ctx of
        [o] -> o -- the new UTxO with the same script address after the state transition
        _   -> traceError "expected exactly one game output"

    -- Returns the `GameDatum` of my own output.
    -- see: https://youtu.be/yczHkTzDnpk?t=879
    outputDatum :: GameDatum
    outputDatum = case gameDatum $ txOutDatumHash ownOutput >>= flip findDatum info of
        Nothing -> traceError "game output datum not found"
        Just d  -> d

    -- Checks if the revealed nonce matches the nonce used to hash the first choice.
    -- `bs` the hash Alice submitted
    -- `nonce` the nonce Alice reveals
    -- `cSecond` the move that both players made. The function will only be involved when Alice knows she has won = both choices are equal (add up to even)
    --
    -- see: https://youtu.be/yczHkTzDnpk?t=1018
    checkNonce :: BuiltinByteString -> BuiltinByteString -> GameChoice -> Bool
    checkNonce bs nonce cSecond = sha2_256 (nonce `appendByteString` cFirst) == bs
      where
        cFirst :: BuiltinByteString
        cFirst = case cSecond of
            Zero -> bsZero'
            One  -> bsOne'

    -- Returns the token to the first player
    -- Attention to the address consisting of a payment part and a staking part
    -- The loser could pay the winnings to the winner
    -- but uses an address where the payment part belongs to the winner but the staking part belongs to himself.
    -- If the winner leaves it at this address the loser would still receive the staking rewards for that. ==> not fixed here.
    -- see: https://youtu.be/yczHkTzDnpk?t=1125
    nftToFirst :: Bool
    nftToFirst = assetClassValueOf (valuePaidTo info $ unPaymentPubKeyHash $ gFirst game) (gToken game) == 1

data Gaming
instance Scripts.ValidatorTypes Gaming where
    type instance DatumType Gaming = GameDatum
    type instance RedeemerType Gaming = GameRedeemer

bsZero, bsOne :: BuiltinByteString
bsZero = "0"
bsOne  = "1"

typedGameValidator :: Game -> Scripts.TypedValidator Gaming
typedGameValidator game = Scripts.mkTypedValidator @Gaming
    ($$(PlutusTx.compile [|| mkGameValidator ||])
        `PlutusTx.applyCode` PlutusTx.liftCode game
        `PlutusTx.applyCode` PlutusTx.liftCode bsZero
        `PlutusTx.applyCode` PlutusTx.liftCode bsOne)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @GameDatum @GameRedeemer

gameValidator :: Game -> Validator
gameValidator = Scripts.validatorScript . typedGameValidator

gameAddress :: Game -> Ledger.Address
gameAddress = scriptAddress . gameValidator

-- see: https://youtu.be/yczHkTzDnpk?t=1708
findGameOutput :: Game -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, GameDatum))
findGameOutput game = do
    utxos <- utxosAt $ gameAddress game
    return $ do
        -- for `find` see: https://youtu.be/yczHkTzDnpk?t=1743
        -- Returns `Just` the element if `f` returns true for the element or `Nothing` otherwise.
        (oref, o) <- find f $ Map.toList utxos
        dat       <- gameDatum $ either (const Nothing) Just $ _ciTxOutDatum o
        return (oref, o, dat)
  where
    f :: (TxOutRef, ChainIndexTxOut) -> Bool
    f (_, o) = assetClassValueOf (_ciTxOutValue o) (gToken game) == 1

-- see: https://youtu.be/yczHkTzDnpk?t=1832
waitUntilTimeHasPassed :: AsContractError e => POSIXTime -> Contract w s e ()
waitUntilTimeHasPassed t = do
    s1 <- currentSlot
    logInfo @String $ "current slot: " ++ show s1 ++ ", waiting until " ++ show t
    void $ awaitTime t >> waitNSlots 1
    s2 <- currentSlot
    logInfo @String $ "waited until: " ++ show s2

data FirstParams = FirstParams
    { fpSecond         :: !PaymentPubKeyHash
    , fpStake          :: !Integer
    , fpPlayDeadline   :: !POSIXTime
    , fpRevealDeadline :: !POSIXTime
    , fpNonce          :: !BuiltinByteString
    , fpCurrency       :: !CurrencySymbol
    , fpTokenName      :: !TokenName
    , fpChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- Contract for the first player
firstGame :: forall w s. FirstParams -> Contract w s Text ()
firstGame fp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let game = Game
            { gFirst          = pkh
            , gSecond         = fpSecond fp
            , gStake          = fpStake fp
            , gPlayDeadline   = fpPlayDeadline fp
            , gRevealDeadline = fpRevealDeadline fp
            , gToken          = AssetClass (fpCurrency fp, fpTokenName fp)
            }
        v    = lovelaceValueOf (fpStake fp) <> assetClassValue (gToken game) 1
        c    = fpChoice fp
        bs   = sha2_256 $ fpNonce fp `appendByteString` if c == Zero then bsZero else bsOne
        -- see: https://youtu.be/yczHkTzDnpk?t=1976
        tx   = Constraints.mustPayToTheScript (GameDatum bs Nothing) v
    ledgerTx <- submitTxConstraints (typedGameValidator game) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "made first move: " ++ show (fpChoice fp)

    waitUntilTimeHasPassed $ fpPlayDeadline fp

    m   <- findGameOutput game
    now <- currentTime
    case m of
        Nothing             -> throwError "game output not found"
        Just (oref, o, dat) -> case dat of
            -- see: https://youtu.be/yczHkTzDnpk?t=2046
            -- The 2nd player hasn't moved.
            GameDatum _ Nothing -> do
                logInfo @String "second player did not play"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData ClaimFirst) <>
                              Constraints.mustValidateIn (from now)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                logInfo @String "reclaimed stake"

            -- see: https://youtu.be/yczHkTzDnpk?t=2082
            -- 2nd player did move
            GameDatum _ (Just c') | c' == c -> do -- same choice => Alice (we / first player) won

                logInfo @String "second player played and lost"
                let lookups = Constraints.unspentOutputs (Map.singleton oref o) <>
                              Constraints.otherScript (gameValidator game)
                    -- reveal nonce to get the winning
                    tx'     = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Reveal $ fpNonce fp) <>
                              Constraints.mustValidateIn (to $ now + 1000)
                ledgerTx' <- submitTxConstraintsWith @Gaming lookups tx'
                void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                logInfo @String "victory"

            _ -> logInfo @String "second player played and won"

data SecondParams = SecondParams
    { spFirst          :: !PaymentPubKeyHash
    , spStake          :: !Integer
    , spPlayDeadline   :: !POSIXTime
    , spRevealDeadline :: !POSIXTime
    , spCurrency       :: !CurrencySymbol
    , spTokenName      :: !TokenName
    , spChoice         :: !GameChoice
    } deriving (Show, Generic, FromJSON, ToJSON, ToSchema)


-- Contract for the second player
-- see: https://youtu.be/yczHkTzDnpk?t=2133
secondGame :: forall w s. SecondParams -> Contract w s Text ()
secondGame sp = do
    pkh <- Contract.ownPaymentPubKeyHash
    let game = Game
            { gFirst          = spFirst sp
            , gSecond         = pkh
            , gStake          = spStake sp
            , gPlayDeadline   = spPlayDeadline sp
            , gRevealDeadline = spRevealDeadline sp
            , gToken          = AssetClass (spCurrency sp, spTokenName sp)
            }
    m <- findGameOutput game
    case m of
        Just (oref, o, GameDatum bs Nothing) -> do
            logInfo @String "running game found"
            now <- currentTime
            let token   = assetClassValue (gToken game) 1
            let v       = let x = lovelaceValueOf (spStake sp) in x <> x <> token
                c       = spChoice sp
                lookups = Constraints.unspentOutputs (Map.singleton oref o)                                   <>
                          Constraints.otherScript (gameValidator game)                                        <>
                          Constraints.typedValidatorLookups (typedGameValidator game)
                tx      = Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData $ Play c) <>
                          Constraints.mustPayToTheScript (GameDatum bs $ Just c) v                            <>
                          Constraints.mustValidateIn (to now)
            ledgerTx <- submitTxConstraintsWith @Gaming lookups tx
            let tid = getCardanoTxId ledgerTx
            void $ awaitTxConfirmed tid
            logInfo @String $ "made second move: " ++ show (spChoice sp)

            waitUntilTimeHasPassed $ spRevealDeadline sp

            m'   <- findGameOutput game
            now' <- currentTime
            case m' of
                Nothing             -> logInfo @String "first player won"
                Just (oref', o', _) -> do
                    logInfo @String "first player didn't reveal" -- -> claim the winning
                    let lookups' = Constraints.unspentOutputs (Map.singleton oref' o')                                     <>
                                   Constraints.otherScript (gameValidator game)
                        tx'      = Constraints.mustSpendScriptOutput oref' (Redeemer $ PlutusTx.toBuiltinData ClaimSecond) <>
                                   Constraints.mustValidateIn (from now')                                                  <>
                                   Constraints.mustPayToPubKey (spFirst sp) (token <> adaValueOf (getAda minAdaTxOut))
                    ledgerTx' <- submitTxConstraintsWith @Gaming lookups' tx'
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx'
                    logInfo @String "second player won"

        _ -> logInfo @String "no running game found"

-- see: https://youtu.be/yczHkTzDnpk?t=2448
type GameSchema = Endpoint "first" FirstParams .\/ Endpoint "second" SecondParams

endpoints :: Contract () GameSchema Text ()
endpoints = awaitPromise (first `select` second) >> endpoints
  where
    first  = endpoint @"first"  firstGame
    second = endpoint @"second" secondGame
